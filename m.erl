
% -module(coffeeMachine).
-module(m).
-compile([export_all]).

% menu wyswielane klientowi
displayMenu() ->
    print({gotoxy, 1, 1}),
    io:format("
            >--------(MENU NAPOJOW)--------<
            ________________________________
            | 1. mala czarna kawa    | 2zl |
            | 2. mala kawa z mlekiem | 3zl |
            | 3. duza czarna kawa    | 3zl |
            | 4. duza kawa z mlekiem | 4zl |
            | 5. kakao               | 4zl |
            ________________________________

Wprowadz pieniadze do automatu: ").

% Magazyn produktow
% woda, kawa, mleko, kakao
productsAtStart() -> {160, 1000, 2000, 100}.

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
        {CID, display_menu} ->
            print({clear}),
            displayMenu(),
            Money = io:get_chars("", 1),

            if Money == "q" -> %w celu testów do szybkiego wychodzenia z programu
                    io:format("KONIEC~n");
                true ->
                    io:format("Wybierz napoj: "),
                    DrinkType = io:get_chars("", 1),
                    CID!{Money, DrinkType},
                    display()
            end;
        {CID, command, Message} ->
            io:format("\e[~p;~pHKOMUNIKAT: ~ts~n", [25, 0, Message]),
            print({gotoxy, 0, 30}),
            timer:sleep(5000),
            self()!{CID, display_menu},
            display()
    end.

% komputer
computer(DisplayID, ProductsID, PaymentID) ->
    receive
        {initialize} ->
            ProductsID!{self(), initialize},
            computer(DisplayID, ProductsID, PaymentID);
        {productsOk} ->
            PaymentID!{self(), initialize},
            computer(DisplayID, ProductsID, PaymentID);
        {paymentTerminalOk} ->
            DisplayID!{self(), display_menu},
            computer(DisplayID, ProductsID, PaymentID);
        {Money, DrinkType} ->
            PaymentID!{self(), isEnough, Money, DrinkType},
            computer(DisplayID, ProductsID, PaymentID);
        {moneyEnough, MoneyInt, DrinkTypeInt} ->
            ProductsID!{self(), MoneyInt, DrinkTypeInt},
            computer(DisplayID, ProductsID, PaymentID);

        {moneyNotEnough, MoneyInt, DrinkTypeInt} ->
            PaymentID!{self(), return_money, MoneyInt, DrinkTypeInt},
            computer(DisplayID, ProductsID, PaymentID);
        {money_returned, MoneyInt, DrinkTypeInt} ->
            DisplayID!{self(), command, "Nie wystarczająca liczba pieniędzy na zakup wybranego produktu! Pieniadze zostaly zwrocone!"},
            computer(DisplayID, ProductsID, PaymentID);

        {productsEnough, MoneyInt, DrinkTypeInt} ->
            io:format("No to robiony bedzie napoj tak oo"),
            timer:sleep(2000),
            self()!{initialize},
            computer(DisplayID, ProductsID, PaymentID)

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
            products({Water, Coffee, Milk, Cocoa});
        {CID, MoneyInt, DrinkTypeInt} ->
            CurrentProducts = productsAtStart(),
            RequiredProducts = requiredProducts(DrinkTypeInt),
            ReqWater = element(1, RequiredProducts),
            ReqCoffee = element(2, RequiredProducts),
            ReqMilk = element(3, RequiredProducts),
            ReqCocoa = element(4, RequiredProducts),

            WaterLeft = Water - ReqWater,
            CoffeeLeft = Coffee - ReqCoffee,
            MilkLeft = Milk - ReqMilk,
            CocoaLeft = Cocoa - ReqCocoa,

            % TUDU:
            % w kazdym casie true trzeba bedzie zmienic na CID!{productsLack, MoneyInt}
            case WaterLeft < 0 of
                false -> null;
                true -> CID!{initialize}, io:format("Nie ma wody ~n"), timer:sleep(2000), products({Water, Coffee, Milk, Cocoa})
            end,

            case CoffeeLeft < 0 of
                false -> null;
                true -> CID!{initialize}, io:format("Nie ma kawy ~n"), timer:sleep(2000), products({Water, Coffee, Milk, Cocoa})
            end,

            case MilkLeft < 0 of
                false -> null;
                true -> CID!{initialize}, io:format("Nie ma mleka ~n"), timer:sleep(2000), products({Water, Coffee, Milk, Cocoa})
            end,

            case CocoaLeft < 0 of
                false -> null;
                true -> CID!{initialize}, io:format("Nie ma kakao ~n"), timer:sleep(2000), products({Water, Coffee, Milk, Cocoa})
            end,

            CID!{productsEnough, MoneyInt, DrinkTypeInt},
            products({WaterLeft, CoffeeLeft, MilkLeft, CocoaLeft})

    end.

start() ->
    DisplayID = spawn(?MODULE, display, []),
    ProductsID = spawn(?MODULE, products, [productsAtStart()]),
    PaymentID = spawn(?MODULE, paymentTerminal, []),
    ComputerID = spawn(?MODULE, computer, [DisplayID, ProductsID, PaymentID]),
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
