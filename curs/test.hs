data List a = Nil | Cons a (List a)

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = 
    Cons x (append xs ys)

data Person = Person {
    firstName :: String, 
    lastName :: String, 
    age :: Int, 
    height :: Float, 
    phoneNumber :: String
}

gigel = Person {
    firstName = "Gigel",
    lastName = "Popescu",
    age = 30,
    height = 1.80,
    phoneNumber = "0722 123 456"
}

mirel = Person {
    firstName = "Mirel",
    lastName = "Ionescu",
    age = 25,
    height = 1.75,
    phoneNumber = "0722 654 321"
}
