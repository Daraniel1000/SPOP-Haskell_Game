module Quest (
      showQuest,
      checkSolution,
      correctSolution,
      correctSolutionComment,
      wrongSolution,
      wrongSolutionComment

) where


import World
import System.IO
import Data.Maybe

questTexts = [
    "0 Slynna pani z telewizji podbiega do Ciebie z mikrofonem. Jesli poprawnie rozwiazesz rebus, mozesz poleciec do Stanow! \n Choc ma grzebien, to nigdy sie nim nie czesze, kto to taki?",
    "1 Nie do wiary! Spotykasz na lotnisku Roberta Maklowicza! Dluga rozmowa o kuchni konczy sie trudnym pytaniem: najpierw mleko czy platki? Co odpowiadasz?: \n mleko \n platki",
    "2 Chcesz na terminalu sprawdzic godzine odlotu. Nagle ekran zaczyna szarzec. Po chwili pojawia sie na nim ciag znakow napisany czcionka Atbash: \n fdzazq \n Potrafisz go rozszyfrowac?",
    "3 O nie, nad lotniskiem szaleje burza. Wszystkie loty odwolane! Musisz wybrac komunikacje zastepcza: \n rower \n kolej",
    "4 Mezczyzna przed Toba upuszcza portfel. Kiedy chcesz mu go zwrocic, zaczyna uciekac! Otwierasz, a w srodku znajdujesz 2 euro oraz kartke z kodem: \n xjmlgcqng\n. Umiesz to rozszyfrowac w iscie cesarskim stylu?",
    "5 Wpada na Ciebie zaplakane dziecko. Mowi, ze ma na jutro do zrobienia zadanie z matematyki, pomozesz mu z rowzwiazaniem? \n x + x + x = 30 \n x + y + y = 20 \n y + z + z = 9 \n y + z * x = ?",
    "6 Kiedy przechodzisz przez bramke, jeden z celnikow pyta, czy znasz sie na informatyce. Wyskoczyl im kod bledu, ale w systemie binarnym: \n 100111101111 \n Ile to bedzie w dziesietnym?",
    "7 Kolejki do odprawy sa bardzo dlugie, dlatego wybierz madrze, w ktorej staniesz: \n 1 \n 2",
    "8 Mijasz grupe turystow, ktorym przewodnik opowiada o grockim historyku Polibiuszu. Po chwili dostajesz SMS'a \n 43-34-44-24-43-55-33-42-51 \n Co kryje sie za ta tajemnicza wiadomoscia?",
    "9 Twoj zegarek zaczyna wariowac! Kolejno pokazuje: \n 14:09 01:26 20:47 11:36 01:19 \n Czy to jakis zegarowy szyfr?",
    "10 Masz troche czasu, mozesz zwiedzic jedna ze slynnych piramid, ktora wybierasz? \n chefrena \n cheopsa \n mykerinosa",
    "11 Czekajac na lot zapoznajesz pewnego biologa, ktory od pewnego czasu rozwaza pewien problem: \n 5 pajakow zjada 5 much w 5 minut, w jakim czasie 50 pajakow zje 50 much?",
    "12 Podchodzi do Ciebie mezczyzna w orientalnej szacie. Proponuje pomoc w podrozy, jesli rozwiazesz pewna zagadke: \n Nie je, nie pije, a chodzi i bije?",
    "13 Pewna pani wyraznie probuje zwrocic Twoja uwage. Kiedy jej sie to udaje, zaczyna na przemian otwierac i zamykac dlon:\n -ooo o- -oo --oo -o-o --oo oo- o--- -o -o-- \n Jaka wiadomosc moze sie kryc za tymi gestami?",
    "14 To byl bardzo dlugi lot, postanawiasz wypic kawe w pobliskiej kawiarnii. Jaki rozmiar kubka wybierasz? \n l \n xl \n xxl",
    "15 Ohayo! Okazuje sie, ze aby wleciec do Japonii nalezy zdac test wiedzy z kultury kraju. Jak nazywaja sie znaki pochodzenia chinskiego w alfabecie japosnkim? ",
    "16 Bardzo zglodniales po tak dlugiej podrozy! Na co masz ochote? \n kaczka \n dim sum \n wonton \n char siu",
    "17 Udajac sie na hale odlotow mijana osoba wciska Ci w reke kartke, na ktorej jest pewne rownanie: \n 2/1 + 2/3 + 4/2 + 4/4 + 1/3 + 1/4 \n co to moze znaczyc?",
    "18 Czekajac na odprawe spotykasz starsza pania z Polski. Prosi o pomoc w przypomnieniu slow piosenki. \n Tyle slonca w calym miescie, \n nie widziales tego jeszcze _____ o _____!"]
questSolutions = ["kogut", "platki", "uwazaj", "rower", "zloniespi", "25", "2543", "2", "ostroznie", "nauka", "cheopsa", "5", "zegar", "badzczujny", "l", "kanji", "kaczka", "blisko", "popatrz"]
questSolvedComments = ["Warszawa - g", "Krakow - g", "Berlin - g", "rower", "Paryz - g", "Bruksela - g", "Wieden - g", "Londyn - g", "Ateny - g", "Moskwa - g", "Kair - g", "Rijad - g", "Nowe Delhi - g", "Hanoi - g", "Sydney - g", "Tokio - g", "Pekin - g", "Seul - g", "Nowy Jork - g"]
questFailedComments = ["Warszawa", "Krakow", "Berlin", "Amsterdam", "Paryz", "Bruksela", "Wieden", "Londyn", "Ateny", "Moskwa", "Kair", "Rijad", "Nowe Delhi", "Hanoi", "Sydney", "Tokio", "Pekin", "Seul", "Nowy Jork"]


showQuest :: Integer -> String
showQuest place = questTexts !! (fromIntegral place ::Int)

checkSolution :: Integer -> String
checkSolution place = questSolutions !! (fromIntegral place ::Int)

correctSolution :: State -> State
correctSolution (place, history, world, moves, goal) = (place, history, world, moves, goal)

correctSolutionComment :: Integer -> String
correctSolutionComment place = questSolvedComments !! (fromIntegral place ::Int) ++ "\n"

wrongSolution :: State -> State
wrongSolution (place, history, world, moves, goal) = (place, history, world, moves, goal)

wrongSolutionComment :: Integer -> String
wrongSolutionComment place = questFailedComments !! (fromIntegral place ::Int) ++ "\n"
