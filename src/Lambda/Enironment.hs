module Lambda.Environment where

import Lambda.Types

import Data.HashTable.IO as H

type HashTable k v = H.CuckooHashTable k v


