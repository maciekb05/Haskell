generatorOperator :: (lewa -> prawa -> wynik) -> lewa -> (prawa -> wynik)
generatorOperator = \func val -> func val
