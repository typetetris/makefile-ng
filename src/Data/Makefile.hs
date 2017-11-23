{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Makefile where

import Data.Text   (Text)
import Data.String (IsString)

newtype Makefile = Makefile { entries :: [Entry] } deriving (Eq, Show)

newtype Target              = Target        Text deriving (Eq, Show, IsString)
newtype NormalDependency    = Normal        Text deriving (Eq, Show, IsString)
newtype OrderOnlyDependency = OrderOnly     Text deriving (Eq, Show, IsString)
newtype VariableName        = VariableName  Text deriving (Eq, Show, IsString)
newtype VariableValue       = VariableValue Text deriving (Eq, Show, IsString)
newtype RecipeLine          = RecipeLine    Text deriving (Eq, Show, IsString)
newtype Comment             = Comment       Text deriving (Eq, Show, IsString)

data Dependencies = Dependencies { normal :: NormalDependency
                                 , orderOnly :: OrderOnlyDependency
                                 }
  deriving (Eq, Show)

data AssignOp = Recursive | Simple | PosixSimple | Append | Conditional | Shell deriving (Eq, Show)

data Entry = SimpleRule Target Dependencies [RecipeLine] Comment
           | PatternRule Target Dependencies [RecipeLine] Comment
           | StaticPatternRule Target Target Dependencies [RecipeLine] Comment
           | VariableAssignment VariableName AssignOp VariableValue Comment
           | CommentLine Comment
           | MultilineVariableAssignment VariableName AssignOp [VariableValue] Comment
  deriving (Eq, Show)
