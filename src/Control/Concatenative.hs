{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PostfixOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Control.Concatenative
    ( (~>),
      (~),
      (~~),
      (~~~),
      dup,
      drop1,
      push,
      pop,
      stack,
      swap,
      rot,
    ) where

import Data.Kind (Type)

data family Stack (a::[Type])

data instance Stack '[] = Nil
data instance Stack (a ': etc) = Cell (Stack etc) a

stack :: Stack '[]
stack = Nil

push :: Stack etc -> b -> Stack (b : etc)
push = (~<)

-- ~< is concatenative/infix `push`
(~<) :: Stack etc -> b -> Stack (b : etc)
s ~< e = Cell s e

dup :: Stack (a:etc) -> Stack (a:a:etc)
dup (Cell prev a) = Cell (Cell prev a) a

swap :: Stack (b:a:etc) -> Stack (a:b:etc)
swap = (><)

-- >< is postfix `swap`
(><) :: Stack (b:a:etc) -> Stack (a:b:etc)
(><) (Cell (Cell prev a) b) = Cell (Cell prev b) a

drop1 :: Stack (a:etc) -> Stack etc
drop1 (Cell prev _) = prev

rot :: Stack (c:b:a:etc) -> Stack (a:c:b:etc)
rot (Cell (Cell (Cell prev a) b) c) = Cell (Cell (Cell prev b) c) a

pop :: Stack (a:etc) -> a
pop = (~>)

-- ~> is postfix `pop`
(~>) :: Stack (a:etc) -> a
(~>) (Cell _ a) = a

(~) :: Stack (a:etc) -> (a -> x) -> Stack(x:etc)
Cell prev a ~ f = Cell prev (f a)

(~~) :: Stack (a:b:etc) -> (b -> a -> x) -> Stack (x:etc)
Cell (Cell prev a) b ~~ f = Cell prev (f a b)

(~~~) :: Stack (a:b:c:etc) -> (c -> b -> a -> x) -> Stack (x:etc)
Cell (Cell (Cell prev a) b) c ~~~ f = Cell prev (f a b c)

-- TODO: Can we collapse multi-arity applications into a Concatenative type-class and single operator?

-- {-# LANGUAGE FunctionalDependencies #-}

-- class Concatenative a f b | a f -> b where
--   (~) :: a -> f -> b

-- instance Concatenative (Stack (a ': etc)) (a -> b) (Stack (b ': etc)) where
--   Cell prev a ~ f = Cell prev (f a)
