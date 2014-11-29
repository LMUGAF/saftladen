module Types ( Product (..)
             , Transaction (..)
             , TransactionType (..)
             , User (..)
             ) where

import Data.Text (Text)

data Product = Product { p_id :: !Text
                       , p_name :: !Text
                       , p_ean :: !Text
                       , p_price :: !Text
                       , p_amount :: !Text
                       , p_volume :: Maybe Text
                       , p_caffeine :: !Text
                       , p_alcohol :: !Text
                       , p_note :: Maybe Text
                       , p_description :: !Text
                       } deriving (Show)


data User = User { u_id :: !Text
                 , u_name :: !Text
                 , u_mail :: Maybe Text
                 , u_gaf_acc :: Maybe Text
                 , u_irc :: Maybe Text
                 , u_total :: !Text
                 , u_note :: !Text
                 , u_enabled :: !Text
                 } deriving (Show)

data TransactionType
  = In
  | Out
  | Buy
  | MoveIn
  | MoveOut
  | Cancel
  deriving (Show)

data Transaction = Transaction { t_id :: !Text
                               , t_product_id :: Maybe Text
                               , t_user_id :: !Text
                               , t_transaction_id :: Maybe Text
                               , t_type :: TransactionType
                               , t_price :: !Text
                               , t_date :: !Text
                               , t_origin :: !Text
                               } deriving (Show)
