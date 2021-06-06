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
    "9 Twoj zegarek zaczyna wariowac! Kolejno pokazuje: \n 21:06 09:05 04:04 00:03 09:02 01:01 23:00 \n Czy to jakis zegarowy szyfr?",
    "10 Z powodu opoznienia lotu masz troche wolnego czasu i mozesz zwiedzic jedna ze slynnych piramid, ktora wybierasz? \n chefrena \n cheopsa \n mykerinosa",
    "11 Czekajac na lot zapoznajesz pewnego biologa, ktory od pewnego czasu rozwaza pewien problem: \n 5 pajakow zjada 5 much w 5 minut, w jakim czasie 50 pajakow zje 50 much?",
    "12 Podchodzi do Ciebie mezczyzna w orientalnej szacie. Proponuje pomoc w podrozy, jesli rozwiazesz pewna zagadke: \n Nie je, nie pije, a chodzi i bije?",
    "13 Pewna pani wyraznie probuje zwrocic Twoja uwage. Kiedy jej sie to udaje, zaczyna na przemian otwierac i zamykac dlon:\n -oo o- o-oo o -o- --- \n Jaka wiadomosc moze sie kryc za tymi gestami?",
    "14 To byl bardzo dlugi lot, postanawiasz wypic kawe w pobliskiej kawiarnii. Jaki rozmiar kubka wybierasz? \n l \n xl \n XXXL",
    "15 Ohayo! Okazuje sie, ze aby wleciec do Japonii nalezy zdac test wiedzy z kultury kraju. Jak nazywaja sie znaki pochodzenia chinskiego w alfabecie japosnkim? ",
    "16 Bardzo zglodniales po tak dlugiej podrozy! Na co masz ochote? \n kaczka \n dim sum \n wonton \n char siu",
    "17 Udajac sie na hale odlotow mijana osoba wciska Ci w reke kartke, na ktorej jest pewne rownanie: \n 2/1 + 2/3 + 4/2 + 4/4 + 1/3 + 1/4 \n co to moze znaczyc?",
    "18 Czekajac na odprawe spotykasz starsza pania z Polski. Prosi o pomoc w przypomnieniu slow piosenki. \n Tyle slonca w calym miescie, \n nie widziales tego jeszcze _____ o _____!"]
questSolutions = ["kogut", "platki", "uwazaj", "rower", "zloniespi", "25", "2543", "2", "ostroznie", "widzimy", "cheopsa", "5", "zegar", "daleko", "l", "kanji", "kaczka", "blisko", "popatrz"]
questSolvedComments = [
    "0 Idealnie! Gratulacje! Lecisz do Nowego Jorku!",
    "1 Robert Maklowicz chwali sobie Twoj gust i chce Ci podarowac swoj bilet do Londynu!",
    "2 Wyglada na to, ze ktos probuje Cie ostrzec. Lepiej badz ostrozny w nastepnych krokach. Dodamy Ci 1 godzine na wykonanie zadania.",
    "3 Para wracajaca z wakacji widzi, jak dzielnie jedziesz w burzy rowerem i sie nad toba lituje. Podwioza Cie do Brukseli!",
    "4 To prawda, dlatego musisz sie spieszyc z dostarczeniem walizki",
    "5 Hurra! Szczesliwe dziecko biegnie pochwalic sie swojemu tacie. Okazuje sie, ze to pilot Twojego samolotu. Za pomoc zyskujesz 4 godziny!",
    "6 Dzieki kodowi bledu udalo sie znalezc rozwiazanie na stack overflow. Ach, te okropne jezyki ze srednikami! Maly znak, a duzy problem. W nagrode zyskujesz dodatkowe 3 godziny",
    "7 Pomimo liczby osob w kolejce, odprawa przebiega zaskakujaco sprawnie i zyskujesz 1 godzine. Milego lotu!",
    "8 Miej oko na walizke, kontragenci moga sie czaic wszedzie. Dodajemy Ci 1 godzine na wykonanie zadania.",
    "9 Dziwnie brzmiaca wiadomosc. Masz pomysl, kto moze obserwowac?",
    "10 Przechodzac miedzy kolejnymi salami tronowymi trafiasz na tablice z hieroglifami. Okazuje sie, ze to pradawny blogoslawienstwo Horusa, zyskujesz z nim dodatkowe 3 godziny lotu!",
    "11 Biolog jest uradowny i informuje Cie o swoich badaniach nad swistakamii w Austrii. Zaprasza Cie na lot sponosorowany przez Towarzystwo Ochrony Lasow do Wiednia!",
    "12 Zgadza sie! To zegar! Mezczyzna informuje, ze jest znanym mentalista. Za rozwiazanie zagadki uczy cie technik slow life, ktore dodaja Ci 5 godzin do czasu wykonania misji.",
    "13 Chciales podejsc do kobiety, jednak ona zniknela w tlumie. Czy to sojusznik, a moze wrog?",
    "14 Idealna ilosc kawy. Teraz czujesz przyplyw energii, ktorego potrzebowales!",
    "15 Yoi! Yoi! To poprawna odpowiedz! Mozesz wleciec do kraju!",
    "16 Kaczka po pekinsku byla pyszna! Jako deser jesz ciasto z wrozba - To bedzie dluga podroz...",
    "17 Dobra wiadomosc! Oby okazala sie prawdziwa!",
    "18 Tak, tak, zgadza sie! Starsza pani za pomoc zdradza Ci kod rabatowy na bilet na samolot lepszej klasy. Zyskujesz na tym rabacie 3 godziny!"]
questFailedComments = [
    "Niestety to nie to. Chodzilo o koguta. Zostajesz w Polsce, ale jako nagrode pocieszenia dostajesz mieszanke wedlowska.",
    "Robertowi nie podoba sie Twoja odpowiedz. Robi Ci wyklad o wyzszosci jednego sposobu przyrzadzania sniadan nad innymi, na ktory tracisz 2 godziny!",
    "Straciles 1 godzine na rozwiazywanie zagadki! Szkoda, ze nie poznales odpowiedzi",
    "Wszyscy podroznicy zdecydowali sie na kolej. Na czekaniu w kolejce do pociagu tracisz az 2 godziny.",
    "Zajety zagadka nie zauwazasz, ze bus odwozacy na plyte lotniska odjechal. Przez to zagapienie opozniasz lot o 1 godzine.",
    "Dziecko jest zalamane tym, ze oboje nie umiecie rozwiazac zadania. Przez nastepne 2 godziny probujesz je pocieszyc",
    "Chyba to nie bylo to, probujecie roznych rozwiazan z interentu, ale okazuja sie nieskuteczne. Tracisz 2 godziny.",
    "Okazuje sie, ze na samym poczatku jest babcia, ktora probowala przemycic na poklad druty do wloczki. Zaczyna opowiadac o dawnych czasach i sluchacie oczarowani przez 2 godziny",
    "8 Ciagle patrzysz sie w telefon szukajac rozwiazania zagadki, tracisz 1 godzine",
    "9 Te zabawy z zegarkiem pozwolily Ci zapomniec o czasie! Tracisz 1 godzine na ponowne ustawienie dobrej godziny.",
    "10 W jednej z sal tronowych uruchamiasz pradawna pulapke! Przez 2 godziny uciekasz przez gignatycznym glazem! Nie zgub walizki!",
    "11 Biolog patrzy w zadumie i nie moze sie doliczyc. Przez nastepna 1 godzine szukacie bledu w jego rachunkach.",
    "12 Chodzilo o zegar! Mezczyzna jest zawiedzony i daje Ci wyklad o czasoprzestrzeni. Uczysz sie o czarnych dziurach przez 2 godziny.",
    "13 Chyba musisz do nastpenej misji potrenowac alfabet Morsa, moze sie przyda.",
    "14 Za duzo, zdecydowanie za duzo kofeiny! Energia Cie tak roznosi, ze w podskokach wsiadasz w zly samolot i lecisz do Londynu!",
    "15 Iie! To nie jest dobra odpowiedz, teraz musisz przejsc 4 godzinny kurs wiedzy o kulturze Japonii.",
    "16 Trzeba bylo wziac kaczke... Ladujesz w toalecie na 2 godziny",
    "17 No trudno! Nastepnym razem moze uda sie rozwiazac! Glowa do gory!",
    "18 Nie, tam chyba bylo inne slowo... Przez 1 godzine spiewacie razem rozne wersje, szukajac odpowiedzi na pytanie."]

questRewards = [0, 0, 1, 0, 0, 4, 3, 1, 1, 0, 3, 0, 5, 0, 0, 0, 0, 0, 3]
questPenalties = [0, 2, 1, 2, 1, 2, 2, 2, 1, 1, 2, 1, 2, 0, 0, 4, 2, 0, 1]


showQuest :: Integer -> String
showQuest place = questTexts !! (fromIntegral place ::Int)

checkSolution :: Integer -> String
checkSolution place = questSolutions !! (fromIntegral place ::Int)

correctSolution :: State -> State
correctSolution (place, history, world, moves, goal) =
    case place of
        0 -> (18, history, world, moves, goal)
        1 -> (7, history, world, moves, goal)
        3 ->  (5, history, world, moves, goal)
        11 ->  (6, history, world, moves, goal)
        otherwise -> (place, history, world, ( moves + questRewards !! (fromIntegral place ::Int) ), goal)


correctSolutionComment :: Integer -> String
correctSolutionComment place = questSolvedComments !! (fromIntegral place ::Int) ++ "\n"

wrongSolution :: State -> State
wrongSolution (place, history, world, moves, goal) =
    case place of
        14 -> (7, history, world, moves, goal)
        otherwise -> (place, history, world, ( moves - questPenalties !! (fromIntegral place ::Int) ), goal)

wrongSolutionComment :: Integer -> String
wrongSolutionComment place = questFailedComments !! (fromIntegral place ::Int) ++ "\n"
