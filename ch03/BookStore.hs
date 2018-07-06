-- file: ch03/BookStore.hs

type CustomerID = Int
type ReviewBody = String

data BookInfo = Book Int String [String]
                deriving (Show)

data MagazineInfo = Magazine Int String [String]
                    deriving (Show)

data BookReview = BookReview BookInfo CustomerID String

data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type BookRecord = (BookInfo, BookReview)

myInfo = Book 9780135072455 "Algebra of Programming"
         ["Richard Bird", "Oege de Moor"]

-- Billing -----------------------------------------------------------

type CardHolder = String
type CardNumber = String
type Address    = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
                 | CashOnDelivery
                 | Invoice CustomerID
                   deriving (Show)

-- Pattern Matching --------------------------------------------------
bookId      (Book id title authors) = id
bookTitle   (Book id title authors) = title
bookAuthors (Book id title authors) = authors

nicerId      (Book id _ _)      = id
nicerTitle   (Book _ title _)   = title
nicerAuthors (Book _ _ authors) = authors

-- Record Syntax -----------------------------------------------------
data Customer = Customer {
  customerID      :: CustomerID,
  customerName    :: String,
  customerAddress :: Address
                         } deriving (Show)

customer1                            = Customer 254234 "TestCustomer" ["nonSense", "Avenue"]
customer2                            = Customer {
                     customerID      = 44564232,
                     customerName    = "Bullshit",
                     customerAddress = ["Unknown"]
                     }