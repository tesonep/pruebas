-----------------------------------------------------------------------------
--
-- Module      :  Object
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
module Object (
    ObjectDef,
    object,
    MethodBody,
    ClassDef
) where

import qualified Data.Map as Map
import Data.Maybe
import Data.UUID
import Data.UUID.V1

{- Object Definition -}


data ObjectDef = ObjectDef {
                    id::UUID,
                    classDef::ClassDef,
                    state::[String]}
                    deriving Show

object = ObjectDef


{- Class Definition -}

data ClassDef = ClassDef{superClass::Maybe ClassDef,
                         className::String,
                         attributes::[Attribute],
                         methods::[Method]}
                         deriving Show

data MethodSignature = MethodSignature {methodName::String,
                                        parameters::[Type],
                                        returnValue::Type} deriving Show

data Method = Method {methodSignature::MethodSignature, methodBody::MethodBody}

instance Show Method where
    show m = methodName' ++ " -> " ++ parameters'  ++ " -> " ++ returnValue'
                where   sig = methodSignature m
                        methodName' = methodName sig
                        parameters' = show $ parameters sig
                        returnValue' = show $ returnValue sig

type MethodBody = ObjectDef -> World (Maybe ObjectDef)

data Attribute = Attribute {attributeName::String,
                            attributeType::Type} deriving Show

data AttributeDef = AttributeDef {attribute::Attribute,
                                  attributeValue::ObjectDef } deriving Show

type Type = [MethodSignature]

emptyType :: Type
emptyType = []

{- World Monad -}

type ObjectMap = Map.Map UUID ObjectDef
type ClassMap = Map.Map String ClassDef

data World  a = World (
                     ObjectMap  ->
                     ClassMap    ->
                     IO (a, ObjectMap, ClassMap))

instance Monad World where
    return x = World (\om cm -> return (x, om, cm))
    (>>=) (World f) f' = World (\om cm ->
                        do
                          (x, om', cm') <- f om cm
                          sacar (f' x) om' cm')

sacar (World f) om cm = f om cm

runWorld (World f) = do
                (a, finalObjectMap, finalClassMap) <- f Map.empty Map.empty
                return a

wrapWorld f = World (f)

registerClass :: ClassDef -> World ()
registerClass def = wrapWorld $ \objectMap classMap ->
                    return ((), objectMap, Map.insert (className def) def classMap)

lookupClass :: String -> World (Maybe ClassDef)
lookupClass className = World (\objectMap classMap -> return (Map.lookup className classMap, objectMap, classMap))


newObject :: ClassDef -> World ObjectDef
newObject classDef = wrapWorld (newObject' classDef)

newObject' classDef objectMap classMap = do
                                   maybeObjectId <- nextUUID
                                   let objectId = fromMaybe nil maybeObjectId in
                                    let o = object objectId classDef [] in
                                        return (o, Map.insert objectId o objectMap, classMap)


{- Base Classes -}

objectClass = ClassDef {
                    superClass=Nothing,
                    className = "Object",
                    attributes = [],
                    methods = []}

integerClass = ClassDef {
                    superClass = Just objectClass,
                    className = "Integer",
                    attributes = [],
                    methods = []}
