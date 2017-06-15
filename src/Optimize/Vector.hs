module Optimize.Vector (module Optimize.Vector, module G) where

import qualified Data.Vector.Generic as G
import qualified Data.Vector.Fusion.Bundle as G
import qualified Data.Vector.Fusion.Bundle.Size as G
import qualified Data.Vector.Fusion.Stream.Monadic as S


mapMaybe :: Monad m => (a -> Maybe b) -> S.Stream m a -> S.Stream m b
mapMaybe f (S.Stream step t) = S.Stream step' t
  where
    step' s = do
                r <- step s
                case r of
                  S.Yield x s' -> do
                                  return $ case f x of
                                    Nothing -> S.Skip s'
                                    Just b' -> S.Yield b' s'
                  S.Skip    s' -> return $ S.Skip s'
                  S.Done       -> return $ S.Done

imapMaybe :: (G.Vector v a, G.Vector v b) => (Int -> a -> Maybe b) -> v a -> v b
{-# INLINE imapMaybe #-}
imapMaybe f = G.unstream
          . G.inplace (mapMaybe (uncurry f) . S.indexed) G.toMax
          . G.stream
