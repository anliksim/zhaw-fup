import Data.List


-------------------------------------------------------------------------------
-- Gegeben ist ein einfacher Datentyp für Verzeichnisstrukturen (Ordnerbäume).
-- Eine Verzeichnisstruktur besteht in diesem Modell entweder aus einem 
-- enzelnen File (durch seinen Namen gegeben), oder durch eine Liste von
-- Unterverzeichnissen.
-------------------------------------------------------------------------------
type Name = String

data FileSystem 
    = File Name 
    | Dir Name [FileSystem]
    deriving Show


example :: FileSystem
example = Dir "root"
    [ Dir "desktop" 
        [ File "notes.txt"
        ]
    , Dir "pictures" 
        [ Dir "holiday2019"
            [ File "paris1.png" 
            , File "paris2.png"
            ]
        , Dir "holiday2018"
            [ File "rome1.jpg"
            , File "rome2.jpg"
            , File "rome3.png"
            ]
        ]
    ]

-- Ein Pfad ist eine Liste von Namen.
-- Beispiel: "/pictures/holiday2019/paris1.png" wäre 
-- ["pictures", "holiday2019", "paris1.png"]
type Path = [Name]

-------------------------------------------------------------------------------
-- Aufgabe: Implementieren Sie die Funktion 'findFS'. Die Funktion soll 
-- bei Angabe eines Pfades die entsprechende Unterstruktur (Ordner oder 
-- File) zurückgeben. Falls der Pfad ungültig ist soll 'Nothing' zurück gegeben
-- werden.

-- Beispiele:

-- Call:   findFS ["pictures","holiday2018","rome1.jpg"] example 
-- Result: Just (File "rome1.jpg") 

-- Call:   findFS ["pictures","holiday2018"] example 
-- Result: Just (Dir "holiday2018" [File "rome1.jpg",File "rome2.jpg",File "rome3.png"]) 

-- Call:   findFS ["pictures","holiday2018","napoli.png"] example 
-- Call:   Nothing 


-------------------------------------------------------------------------------


findFS :: Path -> FileSystem -> Maybe FileSystem
findFS [] fs = undefined -- <- Todo
