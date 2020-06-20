


> module Scientific1
>     (Scientific
>     ,extractInt
>     ,showScientific
>     ,divideScientific
>     ) where

> import Data.Scientific


> extractInt :: Scientific -> Maybe Int
> extractInt n = toBoundedInteger n

> showScientific :: Scientific -> String
> showScientific n = case extractInt n of
>                              Just x -> show x
>                              Nothing ->  show n

jesus

> divideScientific :: Scientific -> Scientific -> Scientific
> divideScientific a b =
>    let a' :: Double
>        a' = Data.Scientific.toRealFloat a
>        b' :: Double
>        b' = Data.Scientific.toRealFloat b
>    in fromFloatDigits (a' / b')
>   
