Adjazenzlisten

Eine passende initiale Adjazenzliste zum Experimentieren:
(Das entspricht dem initialize im BWINF-Portal.)

> adListInit = [  (0, [1]),  (1, [0])  ]

Daraus kann dann mit add diese vollständige Liste werden:

> adListe = 
>  [ (0, [1, 2, 4])
>  , (1, [0, 3])
>  , (2, [0, 3, 4])
>  , (3, [1, 2, 4, 5])
>  , (4, [0, 2, 3, 5])
>  , (5, [3, 4])
>  ]

addEdge - Ali, Yasin
Diese Funktion soll eine neue Kante (Verbindung) zwischen den exisistierenden Knoten schaffen.
  > type AdListe = [(Int, [Int])]

> adListe :: AdListe

> addEdge :: AdListe -> (Int, Int) -> AdListe
> addEdge [] _ = []
> addEdge ((e, ys) : xs) (a, b)
>  | e == a    = (e, b : ys) : addEdge xs (a, b)
>  | e == b    = (e, a : ys) : addEdge xs (a, b)
>  | otherwise = (e, ys)     : addEdge xs (a, b)

Beispiel :
 Eingabe :   addEdge adListe (0, 3)
 Ausgabe : [(0,[3,1,2,4]),(1,[0,3]),(2,[0,3,4]),(3,[0,1,2,4,5]),(4,[0,2,3,5]),(5,[3,4])]
 
 Ich habe das mehrmals ausprobiert sollte klappen 
removeEdge - Bastian, Jason
Die Funktion soll die Kante zwischen zwei Knoten aus der Liste entfernen.

delEdge [] _ _ = []
delEdge ((k, ns) : rest) a b
  | k == a    = (k, delNachbarn ns b) : delEdge rest a b
  | k == b    = (k, delNachbarn ns a) : delEdge rest a b
  | otherwise = (k, ns) : delEdge rest a b

delNachbarn [] _ = []
delNachbarn (x:xs) z
  | x == z    = delNachbarn xs z
  | otherwise = x : delNachbarn xs z


hasEdge - Mustafa, Nuh
Die Funktion soll überprüfen, ob es eine Kante zwischen zwei gegebenen Knoten gibt, also muss diese Funktion einen Boolean wiedergeben. Sie muss sie daher auch zwei Kanten kriegen erhalten als Parameter, wie auch  und den Graphen, der definiert, ob diese verbunden sind oder nicht.

any sucht ein objekt, dass dem übereichten Objekt gleicht (bzw. eine  bestimmte Eigenschaft erfüllt); bei match -> true ansonsten false 
isinList contactx list = any (== contactx) list

-- hasedge wird eine prüfperson überreicht, und eine andere person, zu welcher man bestimmen soll,
-- ob es eine Kante gibt. daher erstmal drei parameter
-- danach überprüft die funktion folgende fälle: ist das erste Element des Paares der Adjazenzliste
--  gleich unserer Prüfperson (client) ist. 
-- Falls ja wird durch die Hilfsfunktion überprüft, ob die Person in der friends-liste dieser Person, 
-- also dem zweiten Element des Tupels, vorkommt. Falls ja, kommt
-- es zu einem Ende der Rekursion und es wird True in einer Liste wiedergegeben, 
-- falls nicht wird ein False widergegeben.

-- Falls das garnicht unsere gesuchte person war, dann wird otherwise
-- eintreten und es würde der Rekursionsschritt folgen und mit xs fortgefahren, und folglich das nächste
-- Tupel der Adjazenzliste rausgenommen. 


hasedge _ _ [] = []
hasedge client contact ((aclient, friends):xs)
 | client == aclient = isinList contact friends : []
 | otherwise = hasedge client contact xs


-- alternativ version ohne any, sondern rekursion

isinList2 _ [] = False  -- Base Case: Element nicht in einer leeren Liste gefunden
isinList2 contactx (x:xs)
 | contactx == x = True  -- Gefunden!
 | otherwise     = isinList2 contactx xs -- Suche im Rest der Liste

hasedge2 _ _ [] = []
hasedge2 client contact ((aclient, friends):xs)
 | client == aclient = isinList2 contact friends : []
 | otherwise = hasedge2 client contact xs

funktioniert


    
getNeighbors - Augustin & Ahmed
Die Funktion sollte, von einem beliebigen Knoten alle Nachbarn angeben. Die Nachbarn sind die Zahlen, die in der Adjazenzliste sind.

getneighbors :: [(Int, [Int])] -> Int -> [Int]
getneighbors [] _ = []
getneighbors ((k, nachbarn):rest) knoten
  | k == knoten = nachbarn
     | otherwise            = getneighbors rest knoten

 numConnections Steven - Farshid 
Die Funktion soll eine Liste der Nachbarknoten (Verbindungspartner) eines angegebenen Knotens erstellen und dann die Länge dieser Liste zählen. 

numConnections :: Int -> Int 
numConnections p = length (getNeighbors p)

promptInvites 

canMessage  David, Dr. Spiele
Die Funktion soll überprüfen, ob sich zwei Benutzer eine Nachricht zusenden können. Dazu wird geprüft, ob Index A und Index B eine Verbindung miteinander haben.  Wir haben eine Person und die adliste. Dann prüfen wir ob die zweite person in der Liste der ersten ist, wenn ja dann True.

canMessage :: 



numberOfMutualConnection Can, Basma, Ranim, Ranna
Die Funktion gibt die Anzahl der indirekten Verbindungen der Knoten zurück. Man kann es sich als gemeinsame Freunde vorstellen.

numMutualConnections

> comparee :: [Int] -> [Int] -> Int
> comparee [] _ = 0
> comparee _ [] = 0
> comparee (x:xs) y = vergleiche x y + comparee xs y

  
> vergleiche _ [] = 0
> vergleiche x (y:ys)
>   | x == y    = 1 + vergleiche x ys
>   | otherwise = 0 + vergleiche x ys

> numMutualConnections x y = compareee x (getNeighbors x adListe) y (getNeighbors y adListe)

> compareee _ [] _ _ = 0
> compareee _ _ _ [] = 0
> compareee personX listeX personY listeY
>   | personX /= personY = comparee listeX listeY
>   | otherwise          = -1 + comparee listeX listeY


Eingabe: numMutualConnections 3 4
Ausgabe: 2

suggestConnections Enes + Farzad
Die Funktion soll dem Nutzer vorschlagen einen Knoten mit einem anderen Knoten zu verbinden (Eine Kante zu erstellen). Wenn Knoten 0, wie oben im Beispiel, eine Kante mit Knoten 1 hat, jedoch keine mit Knoten 3, sollte ein Vorschlag (suggest) kommen, dass man Knoten 0 und 3 verbindet, da Knoten 1 mit beiden Knoten (Knoten 0 und 3) verbunden ist.

> adListe = 
> [ (0, [1, 2, 4])
> , (1, [0, 3])
> , (2, [0, 3, 4])
> , (3, [1, 2, 4, 5])
> , (4, [0, 2, 3, 5])
> , (5, [3, 4])
> ]
>

suggestConnections 0 adListe 
[3,5]

> suggestConnections :: int -> [int]
> suggestConnections p = map getNeighbors (getNeighbors p)
>   | map hasEdge p (suggestConnections) = true     = 
>   | 

Noch nicht fertig und wahrsheinlich falsch









