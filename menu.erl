%  menu.erl

-module(menu).
-compile([export_all]).

%---STALE -> aby uniknac HardCoded Variables
progressBarLength() ->  10.
numberOfProducts() ->    9.
% Stale od numerow lini
progressBarLine() ->    15.
komunikatLine() ->      17.
errorLine() ->          18.
wodaLine()->            20.
kawaLine()->            22.
mlekoLine()->        24.
stanProduktowLine() ->  26.
finishLine() ->         29.
%---STALE END


%Rozne Funkcje-----------------------------------------

% Funkcja zwracajaca składniki poszczegolnych napojow
% woda, kawa, mleko, herbata, kakao
getIngredients(Num) -> 
    element(Num,{
    { 150, 10, 0  , 0, 0 },
    { 300, 25, 0  , 0, 0 },
    { 100, 10, 80 , 0, 0 },
    { 170, 25, 150, 0, 0 },
    { 60 , 20, 200, 0, 0 },
    { 70 , 25, 100, 0, 0 },
    { 50 , 40, 0  , 0, 0 },
    { 250, 0 , 0  , 1, 0 },
    { 0  , 0 , 250, 0, 15}
    }).

% Poczatkowy stan automatu
% woda, kawa, mleko, herbata, kakao
getInitialMachineResources() -> {2000, 1000, 2000, 50, 500}.

%wypisz menu
printMenu() ->
    %print({clear}),
    print({gotoxy, 1, 1}),
    io:format("
            >--------(MENU)--------<
            _________________________
            | mala czarna kawa - 1. |
            | duza czarna kawa - 2. |
            | mala biala kawa  - 3. |
            | duza biala kawa  - 4. |
            | latte            - 5. |
            | capuchino        - 6. |
            | espresso         - 7. |
            | herbata          - 8. |
            | kakao            - 9. |
            _________________________

Wybierz numer napoju: ").

%zwraca pierwszy element listy jako jednoelementowa liste
head([H|_])->[H].

% funkcja dzieki ktorej proces moze wysylac dane cyklicznie.Okreslamy tez ile sygnalow ma wyslac do ktorego procesu. Potrzebne przy paskach postepu etapow przygotowania napoju
wyslijCyklicznieN(_,_,0,{_,_,_}) -> io:format(" ");
wyslijCyklicznieN(Id,Czas,Ile,{A,B,C}) ->
    Id!{A,B,C},
    timer:sleep(Czas),
    wyslijCyklicznieN(Id,Czas,Ile-1,{A,B,C++head(C)}).

%Koniec roznych funkcji-----------------------------------------

%PROCESY ---------------------------------

%proces obslugi monitora WE/WY
monitor() ->
    receive
        {Id, init} ->
            print({clear}),
            timer:sleep(20),
            Id!{monitorOk},
            monitor();
        {Id, start} -> 
            print({clear}),
            printMenu(),
            Napoj = io:get_chars("", 1),
            Id!{Napoj},
            monitor();
        % Komunikat: [tresc komunikatu]
        {Komunikat, komunikat,LineNumber} ->
            io:format("\e[~p;~pHKomunikat: ~p~n", [LineNumber, 0, Komunikat]),
            print({gotoxy,0,finishLine()}),
            monitor();
        % Trzy ponizsze funkcje sluza do wyswietlania informacji typu: [etykieta] [Tekst]
        % string od zmiennej rozni sie sposobem wyswietlania (~s,~p). 
        {etykieta,Linia,Tekst} ->
            io:format("\e[~p;~pH~s", [Linia, 0, Tekst]),
            print({gotoxy,0,finishLine()}),
            monitor();
        {string,Line,Znak} ->
            io:format("\e[~p;~pH ~s", [Line, 7, Znak]),
            print({gotoxy,0,finishLine()}),
            monitor();
        {zmienna,Line,Tekst} ->
            io:format("\e[~p;~pH ~p", [Line, 7, Tekst]),
            print({gotoxy,0,finishLine()}),
            monitor();
        {koniec} ->
            io:format("M koniec ~n")
    end.

%Glowna jednostka odpowiedzialna za obsluge
jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId)->
    receive
        {init} ->
            MonitorId!{self(), init},
            jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        {monitorOk} ->
            MagazynId!{self(), init},
            jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        {magazynOk} ->
            MonitorId!{self(), start},
            jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        %Magazyn ma zasoby na dany napoj
        {magazynMaZasoby,Skladniki} ->
            UsedWoda = element(1,Skladniki),
            UsedKawa = element(2,Skladniki),
            UsedMleko = element(3,Skladniki),

            CzajnikId!{self(),gotujWode,UsedWoda},
            MlynekId!{self(),mielKawe,UsedKawa},
            PodgrzewaczId!{self(),grzejMleko,UsedMleko},
            jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        %Przekazanie do monitora
        {etykieta, Linia, Tekst} ->
            MonitorId!{etykieta,Linia,Tekst},
            jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        {string,Line,Znak} ->
            MonitorId!{string,Line,Znak},
            jednostkaCentralna(MonitorId,MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        {zmienna,Linia,Tekst} ->
            MonitorId!{zmienna,Linia,Tekst},
            jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        %koniec procesu
        {gotowe} ->
            MonitorId!{"Napoj gotowy do odbioru! Dziekujemy za skorzystanie z naszych uslug", komunikat,finishLine()-1},
            timer:sleep(7000),
            MonitorId!{self(), start},
            jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        %komunikaty od jednostek podrzednych
        {woda,zagotowana} ->
            BaristaId!{self(),woda,zagotowana},
            jednostkaCentralna(MonitorId,MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        {kawa,zmielona} ->
            BaristaId!{self(),kawa,zmielona},
            jednostkaCentralna(MonitorId,MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        {mleko,gorace} ->
            BaristaId!{self(),mleko,gorace},
            jednostkaCentralna(MonitorId,MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        % obsluga wejscia uzytkownika
        {"k"} -> 
            exit(self(), "Koniec");
        %reset magazynu
        {"r"} ->
            MagazynId!{self(), init},
            jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        %wyswietli obecny stan na magazynie
        {"s"} ->
            MagazynId!{self(), stan},
            jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        {Napoj} ->
            MonitorId!{"Prosze czekac, przetwarzanie polecenia", komunikat,komunikatLine()},
            MonitorId!{etykieta,wodaLine(),"Woda:"},
            MonitorId!{etykieta,kawaLine(),"Kawa:"},
            MonitorId!{etykieta,mlekoLine(),"Mleko:"},
            %konwersja inputa do inta
            {NumAsInt,_} = string:to_integer(Napoj),

            % obsluga bledu (np: input=a)
            case NumAsInt of
                error -> MonitorId!{"Wprowadzono niepoprawna wartosc", komunikat,errorLine()},
                    timer:sleep(3000),
                    MonitorId!{self(), start},
                    jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
                _ -> null
            end,

            print({gotoxy,0,finishLine()}),
            %komunikacja z magazynem
            MagazynId!{self(), napoj, NumAsInt},
            jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId);
        {Tekst, komunikat, LineNumber} ->
            MonitorId!{Tekst, komunikat,LineNumber},
            jednostkaCentralna(MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId)
    end.

% Jego sens/cel istnienia jest troche skomplikowany dlatego zostanie wyjasniony w dokumentacji
% Po krotce: wie ktore z etapow robienia napoju sa gotowe (zagrzane mleko,woda, zmielona kawa)
% Informacje te sa w nim trzymane, aby nie blokowac JednostkiCentralnej
barista() ->
    receive
           {Id,woda,zagotowana} ->
            receive
                {Id,kawa,zmielona} -> 
                    receive
                        {Id,mleko,gorace} -> Id!{gotowe}, barista()
                    end;
                {Id,mleko,gorace} ->
                    receive
                        {Id,kawa,zmielona} -> Id!{gotowe}, barista()
                    end
            end;
        {Id,kawa,zmielona} ->
            receive
                {Id,woda,zagotowana} -> 
                    receive
                        {Id,mleko,gorace} -> Id!{gotowe}, barista()
                    end;
                {Id,mleko,gorace} ->
                    receive
                        {Id,woda,zagotowana} -> Id!{gotowe}, barista()
                    end
            end;
        {Id,mleko,gorace} ->
            receive
                {Id,kawa,zmielona} -> 
                    receive
                        {Id,woda,zagotowana} -> Id!{gotowe}, barista()
                    end;
                {Id,woda,zagotowana} ->
                    receive
                        {Id,kawa,zmielona} -> Id!{gotowe}, barista()
                    end
            end
    end.

%proces grzania wody
czajnik() ->
    receive
        {Id,gotujWode,Woda} ->
            % ok ile 50ml czesci jest potrzebnych do danego napoju
            Parts = round(Woda/50),
            case Parts of
                %jesli nie potrzeba w ogole to stosowna informacja sie pojawi
                0 -> Id!{string,wodaLine(),"gotowe"};
                % co kwant czasu raportujemy JednostceCentralnej wykonanie czesci zadania
                % uwagi co do sensownosci takiego rozwiazania w dokumentacji
                _ -> wyslijCyklicznieN(Id,Parts*100,progressBarLength(),{string,wodaLine(),"o"})
            end,
            %sygnalizacja konca procesu grzania wody
            Id!{woda,zagotowana},
            czajnik()
    end.

%proces mielenia kawy
%opisy podobne jak przy procesie czajnika
mlynek() ->
    receive
        {Id,mielKawe,Kawa} ->
            Parts = round(Kawa/5),
            case Parts of
                0 -> Id!{string,kawaLine(),"gotowe"};
                _ -> wyslijCyklicznieN(Id,Parts*100,progressBarLength(),{string,kawaLine(),"-"})
            end,
            Id!{kawa,zmielona},
            mlynek()            
    end.

%proces podgrzewania mleka
%opisy podobne jak przy procesie czajnika
podgrzewaczMleka() ->
    receive
        {Id,grzejMleko,Mleko} ->
            Parts = round(Mleko/100),
            case Parts of
                0 -> Id!{string,mlekoLine(),"gotowe"};
                _ -> wyslijCyklicznieN(Id,Parts*100,progressBarLength(),{string,mlekoLine(),">"})
            end,
            Id!{mleko,gorace},
            podgrzewaczMleka()            
    end.

%obsluga magazynu
magazyn(Stan) ->
    %pobranie aktualnych zasobow produktow
    Woda = element(1, Stan),
    Kawa = element(2, Stan),
    Mleko = element(3, Stan),
    Herbata = element(4, Stan),
    Kakao = element(5, Stan),
    receive
        {Id, init} ->
            timer:sleep(20),
            Stan1 = getInitialMachineResources(),
            Id!{magazynOk},
            magazyn(Stan1);
        {Id, stan} ->
            Id!{[Stan], komunikat,komunikatLine()},
            Id!{gotowe},
            magazyn({Woda, Kawa, Mleko, Herbata, Kakao});
        {Id, napoj, NumerNapoju} ->
                %wyciaganie skladnikow danego napoju
                Skladniki = getIngredients(NumerNapoju),
                UsedWoda = element(1,Skladniki),
                UsedKawa = element(2,Skladniki),
                UsedMleko = element(3,Skladniki),
                UsedHerbata = element(4,Skladniki),
                UsedKakao = element(5,Skladniki),
                
                %obliczanie ile zasobow zostanie po produkcji
                WodaLeft = Woda-UsedWoda,
                KawaLeft = Kawa-UsedKawa,
                MlekoLeft = Mleko-UsedMleko,
                HerbataLeft = Herbata-UsedHerbata,
                KakaoLeft = Kakao-UsedKakao,

                %jezeli jakiegos zasobu jest za malo do produkcji bedzie stosowny komunikat
                case WodaLeft<0 of
                    false -> null;
                    true -> Id!{"Brak wody", komunikat,errorLine()},timer:sleep(3000),
                        Id!{gotowe},
                        magazyn({Woda, Kawa, Mleko, Herbata, Kakao})
                end,

                case KawaLeft<0 of
                    false -> null;
                    true -> Id!{"Brak kawy", komunikat,errorLine()},timer:sleep(3000),
                        Id!{gotowe},
                        magazyn({Woda, Kawa, Mleko, Herbata, Kakao})
                end,

                case MlekoLeft<0 of
                    false -> null;
                    true -> Id!{"Brak mleka", komunikat,errorLine()},timer:sleep(3000),
                        Id!{gotowe},
                        magazyn({Woda, Kawa, Mleko, Herbata, Kakao})
                end,

                case HerbataLeft<0 of
                    false -> null;
                    true -> Id!{"brak herbaty", komunikat,errorLine()},timer:sleep(3000),
                        Id!{gotowe},
                        magazyn({Woda, Kawa, Mleko, Herbata, Kakao})
                end,

                case KakaoLeft<0 of
                    false -> null;
                    true -> Id!{"brak kakala", komunikat,errorLine()},timer:sleep(3000),
                        Id!{gotowe},
                        magazyn({Woda, Kawa, Mleko, Herbata, Kakao})
                end,

                %Wyswietlamy stan produktow
                % bardziej zeby pokazac ze magayn dziala- klient nie ma potrzeby wiedziec ile surowcow zostalo w magazynie
                % w klienckiej wersji mozna 2ponizsze linie zakomentowac
                Id!{etykieta,stanProduktowLine(),"Stan: "},
                Id!{zmienna,stanProduktowLine(),{WodaLeft, KawaLeft, MlekoLeft, HerbataLeft, KakaoLeft}},
                %raportujemy JednostceCentralnej ze mamy potrzebne zasoby
                Id!{magazynMaZasoby,{UsedWoda,UsedKawa,UsedMleko,UsedHerbata,UsedKakao}},
                %aktualizacja stanu magazynu
                magazyn({WodaLeft, KawaLeft, MlekoLeft, HerbataLeft, KakaoLeft})
    end.

%---Funkcja glowna---
start() ->
    PodgrzewaczId = spawn(?MODULE,podgrzewaczMleka,[]),
    BaristaId = spawn(?MODULE,barista,[]),
    MlynekId = spawn(?MODULE,mlynek,[]),
    CzajnikId = spawn(?MODULE,czajnik,[]),
    MonitorId = spawn(?MODULE, monitor, []),
    MagazynId = spawn(?MODULE, magazyn, [getInitialMachineResources()]),
    JCid = spawn(?MODULE, jednostkaCentralna, [MonitorId, MagazynId, CzajnikId, MlynekId, BaristaId, PodgrzewaczId]),
    JCid!{init}.

%---Pozostale funkcje---
print({gotoxy,X,Y}) ->
   io:format("\e[~p;~pH",[Y,X]);
print({printxy,X,Y,Msg}) ->
   io:format("\e[~p;~pH~p",[Y,X,Msg]);   
print({clear}) ->
   io:format("\e[2J",[]);
print({tlo}) ->
   print({printxy,2,4,1.2343}),  
   io:format("a",[])  .
   
printxy({X,Y,Msg}) ->
   io:format("\e[~p;~pH~p~n",[Y,X,Msg]). 
