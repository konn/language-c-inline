
{-# LANGUAGE TemplateHaskell, QuasiQuotes, PatternGuards #-}

-- |
-- Module      : Language.C.Inline.ObjC.Marshal
-- Copyright   : [2013] Manuel M T Chakravarty
-- License     : BSD3
--
-- Maintainer  : Manuel M T Chakravarty <chak@cse.unsw.edu.au>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Objective-C-specific marshalling functions.
--
-- FIXME: Some of the code can go into a module for general marshalling, as only some of it is ObjC-specific.

module Language.C.Inline.ObjC.Marshal (
  -- * Auxilliary functions
  determineVarType, checkTypeName,
  
  -- * Determine corresponding foreign types of Haskell types
  haskellTypeToCType, haskellTypeNameToCType,
  
  -- * Marshaller types
  HaskellMarshaller, CMarshaller,
  
  -- * Compute bridging types and marshallers
  generateHaskellToCMarshaller, generateCToHaskellMarshaller
) where

  -- common libraries
import Control.Arrow
import Data.Bits                  (bitSize)
import Data.Char                  (toUpper)
import Data.Int
import Data.Word
import Foreign.C                  as C
import Foreign.C.String           as C
import Foreign.C.Types            as C
import Foreign.Marshal            as C
import Foreign.Ptr                as C
import Foreign.StablePtr          as C
import Foreign.Storable           as C
import Language.Haskell.TH        as TH
import Language.Haskell.TH.Syntax as TH

  -- quasi-quotation libraries
import Language.C.Quote           as QC
import Language.C.Quote.ObjC      as QC
import Text.PrettyPrint.Mainland  as QC

  -- friends
import Language.C.Inline.Error
import Data.Maybe
import Control.Applicative


-- Auxilliary functions
-- --------------------

-- |Check that the given TH name is that of a Haskell variable and determine its type.
--
determineVarType :: TH.Name -> Q TH.Type
determineVarType vname
  = do
    { vinfo <- reify vname
    ; case vinfo of
        VarI _ ty _ _ -> return ty
        nonVarInfo    -> 
          do
          { reportErrorAndFail QC.ObjC $ 
              "expected '" ++ show vname ++ "' to be a variable name, but it is " ++ 
              show (TH.ppr nonVarInfo)
          }
    }

-- |Check that the given TH name is that of a Haskell type constructor.
--
checkTypeName :: TH.Name -> Q ()
checkTypeName tyname
  = do
    { tyinfo <- reify tyname
    ; case tyinfo of
        TyConI (DataD {})    -> return ()
        TyConI (NewtypeD {}) -> return ()
        TyConI (TySynD {})   -> return ()
        nonTyInfo  -> 
          do
          { reportErrorAndFail QC.ObjC $ 
              "expected '" ++ show tyname ++ "' to be a type name, but it is " ++ 
              show (TH.ppr nonTyInfo)
          }
    }


-- Determine foreign types
-- -----------------------

-- | Corresponding C integer type for @Int@.
intTy :: QC.Type
intTy = if bitSize (0::Int) == 64 then [cty| long |] else [cty| int |]
{-# INLINE intTy #-}

cIntTy :: TH.TypeQ
cIntTy = if bitSize (0::Int) == 64 then [t| C.CLong |] else [t| C.CInt |]
{-# INLINE cIntTy #-}

-- | Dictionary for C.Types and corresponding c-type.
cTyMap :: [(TH.Type, QC.Type)]
cTyMap = map (first ConT)
  [(''C.CChar, [cty| char |])
  ,(''C.CSChar, [cty| signed char |])
  ,(''C.CUChar, [cty| unsigned char |])
  ,(''C.CShort, [cty| short |])
  ,(''C.CUShort, [cty| unsigned short |])
  ,(''C.CInt, [cty| int |])
  ,(''C.CUInt, [cty| unsigned int |])
  ,(''C.CLong, [cty| long |])
  ,(''C.CULong, [cty| unsigned long |])
  ,(''C.CLLong, [cty| long long |])
  ,(''C.CULLong, [cty| unsigned long long |])
  ,(''C.CFloat, [cty| float |])
  ,(''C.CDouble, [cty| double |])
  ]

-- | Dictionary for boxed numbers and corresponding c-type.
-- Type Constructor, intermediate Haskell type, corresponding cty, conversion function from cty, to cty.
boxTyMap :: [(TH.Type, (TH.TypeQ, QC.Type, TH.ExpQ, TH.ExpQ))]
boxTyMap = map (first ConT)
  [(''Int, (cIntTy, intTy, [| fromIntegral |], [| fromIntegral |]))
  ,(''Int8, ([t| C.CChar |], [cty| char |], [| fromIntegral |], [| fromIntegral |]))
  ,(''Int16, ([t| C.CShort |], [cty| short |], [| fromIntegral |], [| fromIntegral |]))
  ,(''Int32, ([t| C.CInt |], [cty| int |], [| fromIntegral |], [| fromIntegral |]))
  ,(''Int64, ([t| C.CLong |], [cty| long |], [| fromIntegral |], [| fromIntegral |]))
  ,(''Word8, ([t| C.CUChar |], [cty| unsigned char |], [| fromIntegral |], [| fromIntegral |]))
  ,(''Word16, ([t| C.CUShort |], [cty| unsigned short |], [| fromIntegral |], [| fromIntegral |]))
  ,(''Word32, ([t| C.CUInt |], [cty| unsigned int |], [| fromIntegral |], [| fromIntegral |]))
  ,(''Word64, ([t| C.CULong |], [cty| unsigned long |], [| fromIntegral |], [| fromIntegral |]))
  ,(''Float, ([t| C.CFloat |], [cty| float |], [| \(CFloat a) -> a |], [| CFloat |]))
  ,(''Double, ([t| C.CDouble |], [cty| double |], [| \(CDouble a) -> a |], [| CDouble |]))
  ]

-- | Dictionary of encoding method via NSNumber, used when inside NSArray and so on.
numberMarshalDic :: [(Name, (Q TH.Type, String, QC.Type))]
numberMarshalDic = [(''Int,  if bitSize (0::Int) == 64
                             then ([t| C.CLong |], "long", [cty| long |])
                             else ([t| C.CInt |], "int", [cty| int |]))
                   ,(''CInt, ([t| C.CInt |], "int", [cty| int |]))
                   ,(''CShort, ([t| C.CShort |], "short", [cty|short|]))
                   ,(''CLong, ([t| C.CLong |], "long",[cty|long|]))
                   ,(''Double, ([t| C.CDouble |], "double",[cty|double|]))
                   ,(''CDouble, ([t| C.CDouble |], "double",[cty|double|]))
                   ,(''Float, ([t| C.CFloat |], "float",[cty|float|]))
                   ,(''CFloat, ([t| C.CFloat |],"float",[cty|float|]))
                   ,(''CChar, ([t| C.CChar |], "char",[cty|char|]))
                   ]

-- | Get the Object type for Haskell type.
toObject :: QC.Extensions -> TH.Type -> Maybe QC.Type
toObject lang ty = lookup ty objectDic <|> haskellTypeToCType' lang ty
objectDic = map (first ConT)
  [(''Int, [cty| typename NSNumber * |])
  ,(''Int8, [cty| typename NSNumber * |])
  ,(''Int16, [cty| typename NSNumber * |])
  ,(''Int32, [cty| typename NSNumber * |])
  ,(''Int64, [cty| typename NSNumber * |])
  ,(''Word8, [cty| typename NSNumber * |])
  ,(''Word16, [cty| typename NSNumber * |])
  ,(''Word32, [cty| typename NSNumber * |])
  ,(''Word64, [cty| typename NSNumber * |])
  ,(''Float, [cty| typename NSNumber * |])
  ,(''Double, [cty| typename NSNumber * |])
  ,(''C.CChar, [cty| typename NSNumber * |])
  ,(''C.CSChar, [cty| typename NSNumber * |])
  ,(''C.CUChar, [cty| typename NSNumber * |])
  ,(''C.CShort, [cty| typename NSNumber * |])
  ,(''C.CUShort, [cty| typename NSNumber * |])
  ,(''C.CInt, [cty| typename NSNumber * |])
  ,(''C.CUInt, [cty| typename NSNumber * |])
  ,(''C.CLong, [cty| typename NSNumber * |])
  ,(''C.CULong, [cty| typename NSNumber * |])
  ,(''C.CLLong, [cty| typename NSNumber * |])
  ,(''C.CULLong, [cty| typename NSNumber * |])
  ,(''C.CFloat, [cty| typename NSNumber * |])
  ,(''C.CDouble, [cty| typename NSNumber * |])
  ]


-- |Determine the C type that we map a given Haskell type to.
--
haskellTypeToCType :: QC.Extensions -> TH.Type -> Q QC.Type
haskellTypeToCType lang ty
  = case haskellTypeToCType' lang ty of
      Nothing  -> reportErrorAndFail lang $ "don't know a foreign type suitable for Haskell type '" ++ TH.pprint ty ++ "'"
      Just cty -> return cty

haskellTypeToCType' :: QC.Extensions -> TH.Type -> Maybe QC.Type
haskellTypeToCType' lang (ForallT _tvs _ctxt ty)           -- ignore quantifiers and contexts
  = haskellTypeToCType' lang ty
haskellTypeToCType' lang (ListT `AppT` (ConT charTy))        -- marshal '[Char]' as 'String'
  | charTy == ''Char 
  = haskellTypeNameToCType' lang ''String
haskellTypeToCType' lang (ListT `AppT` argTy)            -- marshal '[a]' as 'NSArray', if we know how to marshal @a@.
  | Just cTy <- toObject lang argTy
  = Just [cty| typename NSArray * |]
haskellTypeToCType' lang (ConT maybeC `AppT` argTy)        -- encode a 'Maybe' around a pointer type in the pointer
  | maybeC == ''Maybe && maybe False isCPtrType cargTy
  = cargTy
  where
    cargTy = haskellTypeToCType' lang argTy
haskellTypeToCType' lang (ConT tc)                         -- nullary type constructors are delegated
  = haskellTypeNameToCType' lang tc
haskellTypeToCType' _lang (VarT _)                         -- can't marshal an unknown type
  = Nothing
haskellTypeCoCType' _lang ty
  | Just cTy <- lookup ty cTyMap = Just cTy
  | otherwise =  Just [cty| typename HsStablePtr |]

-- |Determine the C type that we map a given Haskell type constructor to — i.e., we map all Haskell
-- whose outermost constructor is the given type constructor to the returned C type..
--
haskellTypeNameToCType :: QC.Extensions -> TH.Name -> Q QC.Type
haskellTypeNameToCType ext tyname
  = case haskellTypeNameToCType' ext tyname of
      Nothing  -> reportErrorAndFail ObjC $ "don't know a foreign type suitable for Haskell type '" ++ show tyname ++ "'"
      Just cty -> return cty

haskellTypeNameToCType' :: QC.Extensions -> TH.Name -> Maybe QC.Type
haskellTypeNameToCType' ObjC tyname
  | tyname == ''String  = Just [cty| typename NSString * |]       -- 'String'  -> '(NSString *)'
  | Just cTy <- lookup (ConT tyname) cTyMap           = Just cTy  -- C.Types   -> QC.Types (see 'cTyMap')
  | Just (_,cTy, _,_ ) <- lookup (ConT tyname) boxTyMap = Just cTy  -- BOXED     -> QC.Types (see 'boxTyMap')
  | tyname == ''()      = Just [cty| void |]                      -- '()'      -> 'void'
haskellTypeNameToCType' _lang _tyname                             -- <everything else> -> 'HsStablePtr'
  = Just [cty| typename HsStablePtr |]

-- Check whether the given C type is an overt pointer.
--
isCPtrType :: QC.Type -> Bool
isCPtrType (Type _ (Ptr {}) _)           = True
isCPtrType (Type _ (BlockPtr {}) _)      = True
isCPtrType (Type _ (Array {}) _)         = True
isCPtrType ty
  | ty == [cty| typename HsStablePtr |]  = True
  | otherwise                            = False


-- Determine marshallers and their bridging types
-- ----------------------------------------------

-- |Constructs Haskell code to marshal a value (used to marshal arguments and results).
--
-- * The first argument is the code referring to the value to be marshalled.
-- * The second argument is the continuation that gets the marshalled value as an argument.
--
type HaskellMarshaller = TH.ExpQ -> TH.ExpQ -> TH.ExpQ

-- |Constructs C code to marshal an argument (used to marshal arguments and results).
--
-- * The argument is the identifier of the value to be marshalled.
-- * The result of the generated expression is the marshalled value.
--
type CMarshaller = TH.Name -> QC.Exp

-- |Generate the type-specific marshalling code for Haskell to C land marshalling for a Haskell-C type pair.
--
-- The result has the following components:
--
-- * Haskell type after Haskell-side marshalling.
-- * C type before C-side marshalling.
-- * Generator for the Haskell-side marshalling code.
-- * Generator for the C-side marshalling code.
-- 
generateHaskellToCMarshaller :: TH.Type -> QC.Type -> Q (TH.TypeQ, QC.Type, HaskellMarshaller, CMarshaller)
generateHaskellToCMarshaller hsTy@(ConT maybe `AppT` argTy) cTy
  | maybe == ''Maybe && isCPtrType cTy
  = do 
    { (argTy', cTy', hsMarsh, cMarsh) <- generateHaskellToCMarshaller argTy cTy
    ; ty <- argTy'
    ; case ty of
        ConT ptr `AppT` _ 
          | ptr == ''C.Ptr       -> return ( argTy'
                                           , cTy'
                                           , \val cont -> [| case $val of
                                                               Nothing   -> $cont C.nullPtr
                                                               Just val' -> $(hsMarsh [|val'|] cont) |] 
                                           , cMarsh
                                           )
          | ptr == ''C.StablePtr -> return ( argTy'
                                           , cTy'
                                           , \val cont -> [| case $val of
                                                               Nothing   -> $cont (C.castPtrToStablePtr C.nullPtr)
                                                               Just val' -> $(hsMarsh [|val'|] cont) |]
                                                               -- NB: the above cast works for GHC, but is in the grey area
                                                               --     of the FFI spec
                                           , cMarsh
                                           )
        _ -> reportErrorAndFail ObjC $ "missing 'Maybe' marshalling for '" ++ prettyQC cTy ++ "' to '" ++ TH.pprint hsTy ++ "'"
    }
generateHaskellToCMarshaller hsTy cTy
  | cTy == [cty| typename NSString * |] 
  = return ( [t| C.CString |]
           , [cty| char * |]
           , \val cont -> [| C.withCString $val $cont |]
           , \argName -> [cexp| [NSString stringWithUTF8String: $id:(show argName)] |]
           )
  | cTy == [cty| typename NSNumber * |]
  , Just (intermTy, slotName, cIntermTy) <- lookup hsTy (map (first ConT) numberMarshalDic)
  = do (_, _, tran, unmar) <- generateHaskellToCMarshaller hsTy cIntermTy
       let numberWith = "numberWith" ++ toUpper (head slotName) : tail slotName
       return ( intermTy,
                cIntermTy ,
                tran,
                \argName -> [cexp| [NSNumber $id:numberWith: $exp:(unmar argName)] |])
  | cTy == [cty| typename NSArray * |]
  , ListT `AppT` tv <- hsTy
  , Just fr <- toObject ObjC tv
  = do (inv, intc, tran, unmar) <- generateHaskellToCMarshaller tv fr
       tmpName <- newName "tmp"
       return ( [t| C.Ptr (C.Ptr $inv) |]
              , [cty| $ty:intc ** |]
              , \val cont -> do
                   a <- newName "a"
                   let val' = appsE [[|mapM|], lamE [varP a] $ tran (varE a) [|new|], val]
                   [| do { vals <- $val' ; ans <- withArray0 nullPtr vals $cont
                         ; mapM C.free vals ; return ans}|]
              , \argName -> let arg = show argName in
                 [cexp| ({ NSMutableArray *arr;
                            arr = [NSMutableArray array];
                            int i;i=0;
                            while ($id:arg[i] != NULL) {
                              $ty:intc $id:(show tmpName) = *$id:arg[i++];
                              [arr addObject: $(unmar tmpName)];
                            }
                            [NSArray arrayWithArray: arr];
                          }) |]
              )
  | (hsTy, cTy) `elem` cTyMap
  = return (return hsTy
           , cTy
           , \val cont -> [| $cont $val |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | Just (hsTy', cTy', from, to) <- lookup hsTy boxTyMap
  , cTy' == cTy
  = return (hsTy', cTy, \val cont -> [| $cont ($to $val) |], \argName -> [cexp| $id:(show argName)|])
  | cTy == [cty| typename HsStablePtr |] 
  = return ( [t| C.StablePtr $(return hsTy) |]
           , cTy
           , \val cont -> [| do { C.newStablePtr $val >>= $cont } |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | otherwise
  = reportErrorAndFail ObjC $ "cannot marshal '" ++ TH.pprint hsTy ++ "' to '" ++ prettyQC cTy ++ "'"

-- |Generate the type-specific marshalling code for Haskell to C land marshalling for a C-Haskell type pair.
--
-- The result has the following components:
--
-- * Haskell type after Haskell-side marshalling.
-- * C type before C-side marshalling.
-- * Generator for the Haskell-side marshalling code.
-- * Generator for the C-side marshalling code.
--
generateCToHaskellMarshaller :: TH.Type -> QC.Type -> Q (TH.TypeQ, QC.Type, HaskellMarshaller, CMarshaller)
generateCToHaskellMarshaller hsTy cTy
  | cTy == [cty| typename NSNumber * |]
  , Just (intm, slot, cIntm) <- lookup hsTy (map (first ConT) numberMarshalDic)
  = do (_, _, tran, unmar) <- generateCToHaskellMarshaller hsTy cIntm
       return (intm, cIntm, tran,
               \argName -> [cexp| [$exp:(unmar argName) $id:(slot ++ "Value")] |])
  | cTy == [cty| typename NSString * |]
  = return ( [t| C.CString |]
           , [cty| char * |]
           , \val cont -> [| do { str <- C.peekCString $val; C.free $val; $cont str } |]
           , \argName -> 
               let arg = show argName 
               in
               [cexp|
                 ({ typename NSUInteger maxLen = [$id:arg maximumLengthOfBytesUsingEncoding:NSUTF8StringEncoding] + 1;
                   char *buffer = malloc (maxLen);
                   if (![$id:arg getCString:buffer maxLength:maxLen encoding:NSUTF8StringEncoding])
                     *buffer = '\0';
                   buffer;
                 })
               |]
           )
  | (hsTy, cTy) `elem` cTyMap
  = return (return hsTy, cTy, \val cont -> [| $cont $val |], \argName -> [cexp| $id:(show argName)|])
  | Just (hsTy', cTy', from, to) <- lookup hsTy boxTyMap
  , cTy' == cTy
  = return (hsTy', cTy, \val cont -> [| $cont ($from $val) |], \argName -> [cexp| $id:(show argName)|])
  | cTy == [cty| typename NSArray * |]
  , ListT `AppT` tv <- hsTy
  , Just fr <- toObject ObjC tv
  = do (inv, intc, tran, unmar) <- generateCToHaskellMarshaller tv fr
       tmp <- newName "tmp"
       ptr <- show <$> newName "ptr"
       arrLen <- show <$> newName "arrLen"
       buffer <- show <$> newName "buffer"
       let demote = do { a <- newName "a" ; b <- newName "b"
                       ; lamE [varP a] [| do { ans <- peek $(varE a) >>= $(lamE [varP b] $ tran (varE b) [| return |])
                                             ; return ans} |]
                       }
       return ( [t| C.Ptr (C.Ptr $inv) |]
              , [cty| $ty:intc ** |]
              , \val cont -> [| do { arr0 <- C.peekArray0 nullPtr $val
                                   ; arr' <- mapM $demote arr0
                                   ; $cont arr' } |]
              , \argName -> 
               let arg = show argName
               in [cexp|
                    ({ typename NSUInteger $id:arrLen = [$id:arg count] + 1;
                       $ty:intc **$id:buffer = malloc($id:arrLen);
                       for (int i = 0; i < $id:arrLen - 1; i++) {
                         id $id:(showName tmp) = [$id:arg objectAtIndex: i];
                         $ty:intc *ptr;
                         *ptr = $exp:(unmar tmp);
                         $id:buffer[i] = ptr;
                       }
                       $id:buffer[$id:arrLen - 1] = NULL;
                       $id:buffer;
                    })
                  |]
              )
  | cTy == [cty| typename HsStablePtr |] 
  = return ( [t| C.StablePtr $(return hsTy) |]
           , cTy
           , \val cont -> [| do { C.deRefStablePtr $val >>= $cont } |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | cTy == [cty| void |]
  = return ( [t| () |]
           , [cty| void |]
           , \val cont -> [| $cont $val |]
           , \argName -> [cexp| $id:(show argName) |]
           )
  | otherwise
  = reportErrorAndFail ObjC $ "cannot marshall '" ++ prettyQC cTy ++ "' to '" ++ TH.pprint hsTy ++ "'"    
