module Level04.Types.CommentId
  ( CommentId
  , mkCommentId
  , getCommentId
  ) where

import           Level04.Types.Error (Error)

newtype CommentId = CommentId Int
  deriving (Eq, Show)

mkCommentId :: Int -> Either Error CommentId
mkCommentId t = pure $ CommentId t

getCommentId :: CommentId -> Int
getCommentId (CommentId t) = t
