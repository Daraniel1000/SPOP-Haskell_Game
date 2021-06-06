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

--quests' descriptions
questTexts = [
    "Slynna pani z telewizji podbiega do Ciebie z mikrofonem. Jesli poprawnie rozwiazesz rebus, mozesz poleciec do Stanow! \nChoc ma grzebien, to nigdy sie nim nie czesze, kto to taki?",
    "Nie do wiary! Spotykasz na lotnisku Roberta Maklowicza! Dluga rozmowa o kuchni konczy sie trudnym pytaniem: najpierw mleko czy platki? Co odpowiadasz?: \nmleko \nplatki",
    "Chcesz na terminalu sprawdzic godzine odlotu. Nagle ekran zaczyna szarzec. Po chwili pojawia sie na nim ciag znakow napisany czcionka Atbash:\nfdzazq \nPotrafisz go rozszyfrowac?",
    "O nie, nad lotniskiem szaleje burza. Wszystkie loty odwolane! Musisz wybrac komunikacje zastepcza: \nrower \nkolej",
    "Mezczyzna przed Toba upuszcza portfel. Kiedy chcesz mu go zwrocic, zaczyna uciekac! Otwierasz, a w srodku znajdujesz 2 euro oraz kartke z kodem: \nxjmlgcqng\nUmiesz to rozszyfrowac w iscie cesarskim stylu?",
    "Wpada na Ciebie zaplakane dziecko. Mowi, ze ma na jutro do zrobienia zadanie z matematyki, pomozesz mu z rozwiazaniem? \nx + x + x = 30 \nx + y + y = 20 \ny + z + z = 9 \ny + z * x = ?",
    "Kiedy przechodzisz przez bramke jeden z celnikow pyta, czy znasz sie na informatyce. Wyskoczyl im kod bledu, ale w systemie binarnym: \n100111101111 \nIle to bedzie w dziesietnym?",
    "Kolejki do odprawy sa bardzo dlugie, dlatego wybierz madrze, w ktorej staniesz: \n1 \n2",
    "Mijasz grupe turystow, ktorym przewodnik opowiada o greckim historyku Polibiuszu. Po chwili dostajesz SMS'a \n34-43-44-42-34-55-33-24-15 \nCo kryje sie za ta tajemnicza wiadomoscia?",
    "Twoj zegarek zaczyna wariowac! Kolejno pokazuje: \n21:06 09:05 04:04 00:03 09:02 01:01 23:00 \nCzy to jakis zegarowy szyfr?",
    "Z powodu opoznienia lotu masz troche wolnego czasu i mozesz zwiedzic jedna ze slynnych piramid, ktora wybierasz? \nchefrena \ncheopsa \nmykerinosa",
    "Czekajac na lot poznajesz pewnego biologa, ktory od dawna rozwaza nastepujacy problem: \n5 pajakow zjada 5 much w 5 minut, w jakim czasie 50 pajakow zje 50 much?",
    "Podchodzi do Ciebie mezczyzna w orientalnej szacie. Proponuje pomoc w podrozy, jesli rozwiazesz pewna zagadke: \nNie je, nie pije, a chodzi i bije?",
    "Pewna pani wyraznie probuje zwrocic Twoja uwage. Kiedy jej sie to udaje, zaczyna na przemian otwierac i zamykac dlon:\n-oo o- o-oo o -o- --- \nJaka wiadomosc moze sie kryc za tymi gestami?",
    "To byl bardzo dlugi lot, postanawiasz wypic kawe w pobliskiej kawiarnii. Jaki rozmiar kubka wybierasz? \nl \nxl \nxxxl",
    "Ohayo! Okazuje sie, ze aby wleciec do Japonii nalezy zdac test wiedzy z kultury kraju. Jak nazywaja sie znaki pochodzenia chinskiego w alfabecie japosnkim?",
    "Bardzo zglodniales po tak dlugiej podrozy. Na co masz ochote? \ndim sum \nwonton \nchar siu \nkaczka",
    "Udajac sie na hale odlotow mijana osoba wciska Ci w reke kartke. Widnieje na niej rownanie: \n2/1 + 2/3 + 4/2 + 4/4 + 1/3 + 1/4 \nCo to moze znaczyc?",
    "Czekajac na odprawe spotykasz starsza pania z Polski. Prosi o pomoc w przypomnieniu slow piosenki. \nTyle slonca w calym miescie, \nnie widziales tego jeszcze _____ o _____! Wystarczy jedno slowo."]

--quests' solutions
questSolutions = ["kogut", "platki", "uwazaj", "rower", "zloniespi", "25", "2543", "2", "ostroznie", "widzimy", "cheopsa", "5", "zegar", "daleko", "l", "kanji", "kaczka", "blisko", "popatrz"]

--comments after sloved quests
questSolvedComments = [
    "Idealnie! Gratulacje! Lecisz do Nowego Jorku!",
    "Robert Maklowicz chwali sobie Twoj gust i chce Ci podarowac swoj bilet do Londynu!",
    "Wyglada na to, ze ktos probuje Cie ostrzec. Lepiej badz ostrozny w nastepnych krokach. Dodamy Ci 1 godzine na wykonanie zadania.",
    "Para wracajaca z wakacji widzi, jak dzielnie jedziesz w burzy rowerem i sie nad toba lituje. Podwioza Cie do Brukseli!",
    "To prawda, dlatego musisz sie spieszyc z dostarczeniem przesylki.",
    "Hurra! Szczesliwe dziecko biegnie pochwalic sie swojemu tacie. Okazuje sie, ze to pilot Twojego samolotu. Za pomoc zyskujesz 4 godziny!",
    "Dzieki kodowi bledu udalo sie znalezc rozwiazanie na stack overflow. Ach, te okropne jezyki ze srednikami! Maly znak, a duzy problem. W nagrode zyskujesz dodatkowe 3 godziny.",
    "Pomimo liczby osob w kolejce, odprawa przebiega zaskakujaco sprawnie i zyskujesz 1 godzine. Milego lotu!",
    "Miej oko na przesylke, kontragenci moga sie czaic wszedzie. Dodajemy Ci 1 godzine na wykonanie zadania.",
    "Dziwnie brzmiaca wiadomosc. Masz pomysl, kto moze obserwowac?",
    "Przechodzac miedzy kolejnymi salami tronowymi trafiasz na tablice z hieroglifami. Okazuje sie, ze to pradawne blogoslawienstwo Horusa, zyskujesz z nim dodatkowe 3 godziny.",
    "Biolog jest uradowny i informuje Cie o swoich badaniach nad swistakami w Austrii. Zaprasza Cie na lot sponosorowany przez Towarzystwo Ochrony Lasow do Wiednia.",
    "Zgadza sie! To zegar! Mezczyzna informuje, ze jest znanym mentalista. Za rozwiazanie zagadki uczy cie technik slow life, ktore dodaja Ci 5 godzin do czasu wykonania misji.",
    "Chciales podejsc do kobiety, jednak ona zniknela w tlumie. Czy to sojusznik, a moze wrog?",
    "Idealna ilosc kawy. Teraz czujesz przyplyw energii, ktorego tak potrzebowales!",
    "Yoi! Yoi! To poprawna odpowiedz! Mozesz wleciec do kraju!",
    "Kaczka po pekinsku byla pyszna! Jako deser jesz ciasteczko z wrozba - To bedzie dluga podroz...",
    "Dobra wiadomosc! Oby okazala sie prawdziwa!",
    "Tak, tak, zgadza sie! Starsza pani za pomoc zdradza Ci kod rabatowy na bilet na samolot lepszej klasy. Zyskujesz na tym rabacie 3 godziny."]

--comments after failed quests
questFailedComments = [
    "Niestety, to nie to. Chodzilo o koguta. Zostajesz w Polsce, ale jako nagrode pocieszenia dostajesz mieszanke wedlowska.",
    "Robertowi nie podoba sie Twoja odpowiedz. Robi Ci wyklad o wyzszosci dolewania mleka do platek, aby byly chrupiace. Tracisz 2 godziny.",
    "Straciles 1 godzine na rozwiazywanie zagadki! Szkoda, ze nie poznales odpowiedzi.",
    "Wszyscy podroznicy zdecydowali sie na kolej. Na czekaniu w kolejce do pociagu tracisz az 2 godziny.",
    "Zajety zagadka nie zauwazasz, ze bus odwozacy na plyte lotniska odjechal. Przez to zagapienie opozniasz lot o 1 godzine.",
    "Dziecko jest zalamane tym, ze oboje nie umiecie rozwiazac zadania. Przez nastepne 2 godziny probujesz je pocieszyc",
    "Probujecie roznych rozwiazan z interentu, ale okazuja sie nieskuteczne. Tracisz 2 godziny.",
    "Okazuje sie, ze na samym poczatku jest babcia, ktora probowala przemycic na poklad druty do wloczki. Zaczyna opowiadac o dawnych czasach. Sluchacie oczarowani przez 2 godziny.",
    "Ciagle patrzysz sie w telefon szukajac rozwiazania zagadki. Tracisz 1 godzine.",
    "Te zabawy z zegarkiem pozwolily Ci zapomniec o czasie! Tracisz 1 godzine na ponowne ustawienie dobrej godziny.",
    "W jednej z sal tronowych uruchamiasz pradawna pulapke! Przez 2 godziny uciekasz przez gignatycznym glazem! Nie zgub przesylki!",
    "Biolog patrzy w zadumie i nie moze sie doliczyc. Przez nastepna 1 godzine szukacie bledu w jego rachunkach.",
    "Chodzilo o zegar! Mezczyzna jest zawiedzony i daje Ci wyklad o czasoprzestrzeni. Uczysz sie o czarnych dziurach przez 2 godziny.",
    "Chyba musisz do nastpenej misji potrenowac alfabet Morsa, moze sie przyda.",
    "Za duzo, zdecydowanie za duzo kofeiny! Energia Cie tak roznosi, ze w podskokach wsiadasz do zlego samolotu i lecisz do Londynu!",
    "Iie! To nie jest dobra odpowiedz, teraz musisz przejsc 4 godzinny kurs wiedzy o kulturze Japonii.",
    "Trzeba bylo wziac kaczke... Ladujesz w toalecie na 2 godziny.",
    "No trudno! Nastepnym razem moze uda sie rozwiazac! Glowa do gory!",
    "Nie, tam chyba bylo inne slowo... Przez 1 godzine spiewacie razem rozne wersje, szukajac odpowiedzi na pytanie."]

--additional time after solved quests
questRewards = [0, 0, 1, 0, 0, 4, 3, 1, 1, 0, 3, 0, 5, 0, 0, 0, 0, 0, 3]

--penatly time after failed quests
questPenalties = [0, 2, 1, 2, 1, 2, 2, 2, 1, 1, 2, 1, 2, 0, 0, 4, 2, 0, 1]

--function returns quest's description
showQuest :: Integer -> String
showQuest place = questTexts !! (fromIntegral place ::Int)

--function returns quest's solution
checkSolution :: Integer -> String
checkSolution place = questSolutions !! (fromIntegral place ::Int)


--function returns game's state after solved quest
correctSolution :: State -> State
correctSolution (place, history, world, moves, goal) =
    case place of
        0 -> (18, history, world, moves, goal)
        1 -> (7, history, world, moves, goal)
        3 ->  (5, history, world, moves, goal)
        11 ->  (6, history, world, moves, goal)
        otherwise -> (place, history, world, ( moves + questRewards !! (fromIntegral place ::Int) ), goal)

--function returns proper comment after solved quest
correctSolutionComment :: Integer -> String
correctSolutionComment place = questSolvedComments !! (fromIntegral place ::Int) ++ "\n"

--function returns game's state after failed quest
wrongSolution :: State -> State
wrongSolution (place, history, world, moves, goal) =
    case place of
        14 -> (7, history, world, moves, goal)
        otherwise -> (place, history, world, ( moves - questPenalties !! (fromIntegral place ::Int) ), goal)

--function returns proper comment after failed quest
wrongSolutionComment :: Integer -> String
wrongSolutionComment place = questFailedComments !! (fromIntegral place ::Int) ++ "\n"
