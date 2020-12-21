
% -module(coffeeMachine).
-module(m).
-compile([export_all]).

% menu wyswielane klientowi
displayMenu(CurrentState) ->
    print({gotoxy, 1, 1}),
    io:format("
    ===== COFFEE MACHINE MENU  =====
    ================================
    | 1. mala czarna kawa    | 2zl |
    | 2. mala kawa z mlekiem | 3zl |
    | 3. duza czarna kawa    | 3zl |
    | 4. duza kawa z mlekiem | 4zl |
    | 5. kakao               | 4zl |
    ================================

Obecny stan produktow: ~p
__________________________________________

Wprowadz pieniadze do automatu: ",[CurrentState]).

% Magazyn produktow
% woda, kawa, mleko, kakao
productsAtStart() -> {1600, 1000, 2000, 100}.

head([H|_])->[H].

% progress
progressPrinter(_,_,0,{_,_,_}) -> io:format(" ");
progressPrinter(Id,Czas,Ile,{A,B,C}) ->
    Id!{A,B,C},
    timer:sleep(Czas),
    progressPrinter(Id,Czas,Ile-1,{A,B,C++head(C)}).

% Baza produktow potrzebnych do wytworzenia napojow
% woda, kawa, mleko, kakao, pieniadze
requiredProducts(DrinkNum) ->
    element(DrinkNum,{
    { 150, 15, 0  , 0, 2 },
    { 100, 15, 90  , 0, 3 },
    { 170, 30, 0 , 0, 3 },
    { 120, 30, 120, 0, 4 },
    { 0  , 0 , 250, 15, 4}
    }).

% no display
display() ->
    receive
        {CID, display_menu, CurrentState} ->
            print({clear}),
            displayMenu(CurrentState),
            Money = io:get_chars("", 1),

            if Money == "q" -> %w celu testów do szybkiego wychodzenia z programu
                    io:format("KONIEC~n");
                true ->
                    io:format("Wybierz napoj: "),
                    DrinkType = io:get_chars("", 1),
                    io:format("__________________________________________~n~n"),
                    CID!{choice, Money, DrinkType},
                    display()
            end;
        {command, Message} ->
            io:format("\e[~p;~pHKOMUNIKAT: ~ts~n", [29, 0, Message]),
            print({gotoxy, 0, 30}),
            % timer:sleep(5000),
            % % self()!{CID, display_menu},
            display();
        {char,Line,Char} ->
            io:format("\e[~p;~pH ~s", [Line, 13, Char]),
            print({gotoxy,0,30}),
            display();
        {string, Line, Message} ->
            io:format("\e[~p;~pH~ts", [Line, 5, Message]),
            print({gotoxy, 0, 30}),
            display()

    end.

% komputer
computer(DisplayID, ProductsID, PaymentID, WaterHeaterID) ->
    receive
        {initialize} ->
            ProductsID!{self(), initialize},
            computer(DisplayID, ProductsID, PaymentID, WaterHeaterID);
        {productsOk} ->
            PaymentID!{self(), initialize},
            computer(DisplayID, ProductsID, PaymentID, WaterHeaterID);
        {paymentTerminalOk} ->
            self()!{print_menu},
            computer(DisplayID, ProductsID, PaymentID, WaterHeaterID);
        {print_menu} ->
            ProductsID!{self(), get_current_state},
                receive
                    {current_state, State} ->
                        CurrentState = State
                end,
            DisplayID!{self(), display_menu, CurrentState},
            computer(DisplayID, ProductsID, PaymentID, WaterHeaterID);
        {choice, Money, DrinkType} ->
            PaymentID!{self(), isEnough, Money, DrinkType},
            computer(DisplayID, ProductsID, PaymentID, WaterHeaterID);
        {moneyEnough, MoneyInt, DrinkTypeInt} ->
            ProductsID!{self(), isEnough, MoneyInt, DrinkTypeInt},
            computer(DisplayID, ProductsID, PaymentID, WaterHeaterID);
        {productsEnough, MoneyInt, DrinkTypeInt} ->
            PaymentID!{self(), return_rest, MoneyInt, DrinkTypeInt},
            computer(DisplayID, ProductsID, PaymentID, WaterHeaterID);
        {payment_ok, DrinkTypeInt} ->
            io:format("Trwa przygotowywanie napoju...~n~n"),
            ProductsID!{self(), get, DrinkTypeInt},
            receive
                {products_got, ReqWater, ReqCoffee, ReqMilk, ReqCocoa} ->
                    % 18 woda, 19 kawa , 20 mleko, 22 mikser,
                    DisplayID!{string, 22, "woda: "},
                    DisplayID!{string, 23, "kawa: "},
                    DisplayID!{string, 24, "mleko: "},
                    DisplayID!{string, 26, "mikser: "},

                    WaterHeaterID!{self(), DisplayID, heat_water, ReqWater}

                    % to się wykona jak wszystko zostanie wykonane
                    % DisplayID!{command, "Napój gotowy :) Dziękujemy!"},
                    % timer:sleep(5000),
                    % self()!{print_menu}
            end,

            computer(DisplayID, ProductsID, PaymentID, WaterHeaterID);

        {water, heated} ->
            DisplayID!{command, "Napój gotowy :) Dziękujemy!"},
            timer:sleep(5000),
            self()!{print_menu},
            computer(DisplayID, ProductsID, PaymentID, WaterHeaterID);

        {moneyNotEnough, MoneyInt, DrinkTypeInt} ->
            PaymentID!{self(), return_money, MoneyInt, DrinkTypeInt},
            receive
                {money_returned, MoneyInt, DrinkTypeInt} ->
                    DisplayID!{command, "Nie wystarczająca liczba pieniędzy na zakup wybranego produktu! Pieniadze zostaly zwrocone!"},
                    timer:sleep(5000),
                    self()!{print_menu}
            end,
            computer(DisplayID, ProductsID, PaymentID, WaterHeaterID);

        {products_lack, MoneyInt, DrinkTypeInt} ->
            PaymentID!{self(), return_money, MoneyInt, DrinkTypeInt},
            receive
                {money_returned, MoneyInt, DrinkTypeInt} ->
                    DisplayID!{command, "Nie wystarczająca ilość produktów w maszynie :( Pieniadze zostaly zwrocone!"},
                    timer:sleep(5000),
                    self()!{print_menu}
            end,
            computer(DisplayID, ProductsID, PaymentID, WaterHeaterID)
    end.

paymentTerminal() ->
    receive
        {CID, initialize} ->
            CID!{paymentTerminalOk},
            paymentTerminal();
        {CID, isEnough, Money, DrinkType} ->
            {MoneyInt,_} = string:to_integer(Money),
            {DrinkTypeInt,_} = string:to_integer(DrinkType),
            ProductsNeeded = requiredProducts(DrinkTypeInt),
            MoneyNeeded = element(5, ProductsNeeded),

            if MoneyInt >= MoneyNeeded ->
                CID!{moneyEnough, MoneyInt, DrinkTypeInt};
            true ->
                CID!{moneyNotEnough, MoneyInt, DrinkTypeInt}
            end,
            paymentTerminal();

        {CID, return_rest, MoneyInt, DrinkTypeInt} ->
            ProductsNeeded = requiredProducts(DrinkTypeInt),
            MoneyNeeded = element(5, ProductsNeeded),
            Rest = MoneyInt - MoneyNeeded,
            io:format("paymentTerminal -> #ZWRACAM RESZTE: ~pzl#~n", [Rest]),
            CID!{payment_ok, DrinkTypeInt},
            paymentTerminal();

        {CID, return_money, MoneyInt, DrinkTypeInt} ->
            io:format("~n~n~n~npaymentTerminal -> #ZWRACAM PIENIADZE#~n~n"),
            CID!{money_returned, MoneyInt, DrinkTypeInt},
            paymentTerminal()
    end.

products(ProductsLeft) ->
    Water = element(1, ProductsLeft),
    Coffee = element(2, ProductsLeft),
    Milk = element(3, ProductsLeft),
    Cocoa = element(4, ProductsLeft),
    receive
        {CID, initialize} ->
            CID!{productsOk},
            products(ProductsLeft);
        {CID, get_current_state} ->
            CID!{current_state, ProductsLeft},
            products(ProductsLeft);
        {CID, isEnough, MoneyInt, DrinkTypeInt} ->
            RequiredProducts = requiredProducts(DrinkTypeInt),
            ReqWater = element(1, RequiredProducts),
            ReqCoffee = element(2, RequiredProducts),
            ReqMilk = element(3, RequiredProducts),
            ReqCocoa = element(4, RequiredProducts),

            WaterLeft = Water - ReqWater,
            CoffeeLeft = Coffee - ReqCoffee,
            MilkLeft = Milk - ReqMilk,
            CocoaLeft = Cocoa - ReqCocoa,


            case WaterLeft < 0 of
                false -> null;
                true ->
                    CID!{products_lack, MoneyInt, DrinkTypeInt},
                    io:format("Nie ma wody ~n"),
                    timer:sleep(2000),
                    products({Water, Coffee, Milk, Cocoa})
            end,

            case CoffeeLeft < 0 of
                false -> null;
                true ->
                    CID!{products_lack, MoneyInt, DrinkTypeInt},
                    io:format("Nie ma kawy ~n"),
                    timer:sleep(2000),
                    products({Water, Coffee, Milk, Cocoa})
            end,

            case MilkLeft < 0 of
                false -> null;
                true ->
                    CID!{products_lack, MoneyInt, DrinkTypeInt},
                    io:format("Nie ma mleka ~n"),
                    timer:sleep(2000),
                    products({Water, Coffee, Milk, Cocoa})
            end,

            case CocoaLeft < 0 of
                false -> null;
                true ->
                    CID!{products_lack, MoneyInt, DrinkTypeInt},
                    io:format("Nie ma kakao ~n"),
                    timer:sleep(2000),
                    products({Water, Coffee, Milk, Cocoa})
            end,

            CID!{productsEnough, MoneyInt, DrinkTypeInt},
            products(ProductsLeft);

        {CID, get, DrinkTypeInt} ->
            RequiredProducts = requiredProducts(DrinkTypeInt),
            ReqWater = element(1, RequiredProducts),
            ReqCoffee = element(2, RequiredProducts),
            ReqMilk = element(3, RequiredProducts),
            ReqCocoa = element(4, RequiredProducts),

            WaterLeft = Water - ReqWater,
            CoffeeLeft = Coffee - ReqCoffee,
            MilkLeft = Milk - ReqMilk,
            CocoaLeft = Cocoa - ReqCocoa,
            CID!{products_got, ReqWater, ReqCoffee, ReqMilk, ReqCocoa},
            products({WaterLeft, CoffeeLeft, MilkLeft, CocoaLeft})

    end.

waterHeater() ->
    receive
        {CID, DID, heat_water, Amount} ->
            Parts = round(Amount/25),
            case Parts of
                0 -> DID!{string,22,"gotowe"};
                _ -> progressPrinter(DID,Parts*100,10,{char,22,"="})
            end,
            CID!{water, heated},
            waterHeater()
    end.

start() ->
    DisplayID = spawn(?MODULE, display, []),
    WaterHeaterID = spawn(?MODULE, waterHeater, []),
    ProductsID = spawn(?MODULE, products, [productsAtStart()]),
    PaymentID = spawn(?MODULE, paymentTerminal, []),
    ComputerID = spawn(?MODULE, computer, [DisplayID, ProductsID, PaymentID, WaterHeaterID]),
    ComputerID!{initialize}.


% Funkcje pomocnicze
print({gotoxy,X,Y}) ->
   io:format("\e[~p;~pH",[Y,X]);
print({printxy,X,Y,Msg}) ->
   io:format("\e[~p;~pH~p",[Y,X,Msg]);
print({clear}) ->
   io:format("\ec",[]).
printxy({X,Y,Msg}) ->
   io:format("\e[~p;~pH~p~n",[Y,X,Msg]).
