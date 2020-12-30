-module(coffeeMachine).
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
productsAtStart() -> {5, 5, 5, 5}.

% Baza produktow potrzebnych do wytworzenia napojow
% woda, kawa, mleko, kakao, pieniadze
requiredProducts(DrinkNum) ->
    element(DrinkNum,{
    { 1, 1, 0, 0, 2 },
    { 1, 1, 1, 0, 3 },
    { 2, 2, 0, 0, 3 },
    { 2, 2, 2, 0, 4 },
    { 0, 0, 4, 1, 4}
    }).

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
            display();

        {update_display, Line, Progress} ->
            io:format("\e[~p;~pH ~ts", [Line, 13, Progress]),
            print({gotoxy,0,30}),
            display();

        {string, Line, Message} ->
            io:format("\e[~p;~pH~ts", [Line, 5, Message]),
            print({gotoxy, 0, 30}),
            display()

    end.

% komputer
computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances) ->
    receive
        {initialize} ->
            ProductsID!{self(), initialize},
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {products_ok} ->
            PaymentID!{self(), initialize},
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {payment_terminal_ok} ->
            self()!{print_menu},
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {print_menu} ->
            ProductsID!{self(), get_current_state},
                receive
                    {current_state, State} ->
                        CurrentState = State
                end,
            DisplayID!{self(), display_menu, CurrentState},
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {choice, Money, DrinkType} ->
            PaymentID!{self(), is_enough, Money, DrinkType},
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {money_enough, MoneyInt, DrinkTypeInt} ->
            ProductsID!{self(), is_enough, MoneyInt, DrinkTypeInt},
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {products_enough, MoneyInt, DrinkTypeInt} ->
            PaymentID!{self(), return_rest, MoneyInt, DrinkTypeInt},
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {payment_ok, DrinkTypeInt} ->
            io:format("Trwa przygotowywanie napoju...~n~n"),
            ProductsID!{self(), get, DrinkTypeInt},
            receive
                {products_got, ReqWater, ReqCoffee, ReqMilk, _} ->
                    % 22 woda, 23 kawa , 24 mleko, 26 mikser,
                    DisplayID!{string, 22, "woda: "},
                    DisplayID!{string, 23, "kawa: "},
                    DisplayID!{string, 24, "mleko: "},
                    DisplayID!{string, 26, "mikser: "},

                    WaterHeaterID = element(1, Appliances),
                    MilkHeaterID = element(2, Appliances),
                    CoffeeGrinderID= element(3, Appliances),

                    WaterHeaterID!{self(), MonitorID, heat_water, ReqWater},
                    MilkHeaterID!{self(), MonitorID, heat_milk, ReqMilk},
                    CoffeeGrinderID!{self(), MonitorID, grind_coffee, ReqCoffee},

                    MonitorID!{self(), cocoa_poured}
            end,
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {update_progress, Line, Progress} ->
            DisplayID!{update_display, Line, "[" ++ Progress ++ "]"},
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {unused_product, Line} ->
            DisplayID!{update_display, Line, "[nie zawiera]"},
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {drink_ready_to_mix} ->
            MixerID = element(4, Appliances),
            MixerID!{self(), start_mixing},
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {drink_is_mixing} ->
            DisplayID!{update_display, 26, "mieszanie..."},
             computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {drink_ready} ->
            DisplayID!{update_display, 26, "wymieszane"},
            timer:sleep(1000),
            DisplayID!{command, "Napój gotowy :) Dziękujemy!"},
            timer:sleep(5000),
            self()!{print_menu},
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {money_not_enough, MoneyInt, DrinkTypeInt} ->
            PaymentID!{self(), return_money, MoneyInt, DrinkTypeInt},
            receive
                {money_returned, MoneyInt, DrinkTypeInt} ->
                    DisplayID!{command, "Nie wystarczająca liczba pieniędzy na zakup wybranego produktu! Pieniadze zostaly zwrocone!"},
                    timer:sleep(5000),
                    self()!{print_menu}
            end,
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances);

        {products_lack, MoneyInt, DrinkTypeInt} ->
            PaymentID!{self(), return_money, MoneyInt, DrinkTypeInt},
            receive
                {money_returned, MoneyInt, DrinkTypeInt} ->
                    DisplayID!{command, "Nie wystarczająca ilość produktów w maszynie :( Pieniadze zostaly zwrocone!"},
                    timer:sleep(5000),
                    self()!{print_menu}
            end,
            computer(DisplayID, ProductsID, PaymentID, MonitorID, Appliances)
    end.

paymentTerminal() ->
    receive
        {CID, initialize} ->
            CID!{payment_terminal_ok},
            paymentTerminal();

        {CID, is_enough, Money, DrinkType} ->
            {MoneyInt,_} = string:to_integer(Money),
            {DrinkTypeInt,_} = string:to_integer(DrinkType),
            ProductsNeeded = requiredProducts(DrinkTypeInt),
            MoneyNeeded = element(5, ProductsNeeded),

            if MoneyInt >= MoneyNeeded ->
                CID!{money_enough, MoneyInt, DrinkTypeInt};
            true ->
                CID!{money_not_enough, MoneyInt, DrinkTypeInt}
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
            CID!{products_ok},
            products(ProductsLeft);

        {CID, get_current_state} ->
            CID!{current_state, ProductsLeft},
            products(ProductsLeft);

        {CID, is_enough, MoneyInt, DrinkTypeInt} ->
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
                    products({Water, Coffee, Milk, Cocoa})
            end,

            case CoffeeLeft < 0 of
                false -> null;
                true ->
                    CID!{products_lack, MoneyInt, DrinkTypeInt},
                    products({Water, Coffee, Milk, Cocoa})
            end,

            case MilkLeft < 0 of
                false -> null;
                true ->
                    CID!{products_lack, MoneyInt, DrinkTypeInt},
                    products({Water, Coffee, Milk, Cocoa})
            end,

            case CocoaLeft < 0 of
                false -> null;
                true ->
                    CID!{products_lack, MoneyInt, DrinkTypeInt},
                    products({Water, Coffee, Milk, Cocoa})
            end,

            CID!{products_enough, MoneyInt, DrinkTypeInt},
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

head([H|_])->[H].
procesSimulator(_, 0, _, {_, _}) -> null;
procesSimulator(CID, HowMany, TimeOfSequence, {Line, Progress}) ->
    CID!{update_progress, Line, Progress},
    timer:sleep(TimeOfSequence),
    procesSimulator(CID, HowMany - 1, TimeOfSequence, {Line, Progress ++ head(Progress)}).


waterHeater() ->
    receive
        {CID, MID, heat_water, Amount} ->
            Times = Amount*5,
            case Times of
                0 -> CID!{unused_product, 22};
                _ -> procesSimulator(CID, Times, 500, {22,"="})
            end,
            MID!{CID, water_heated},
            waterHeater()
    end.

coffeeGrinder() ->
    receive
        {CID, MID, grind_coffee, Amount} ->
            Times = Amount*5,
            case Times of
                0 -> CID!{unused_product, 23};
                _ -> procesSimulator(CID, Times, 300, {23,"="})
            end,
            MID!{CID, coffee_grinded},
            coffeeGrinder()
    end.

milkHeater() ->
    receive
        {CID, MID, heat_milk, Amount} ->
            Times = Amount*5,
            case Times of
                0 -> CID!{unused_product, 24};
                _ -> procesSimulator(CID, Times, 600, {24,"="})
            end,
            MID!{CID, milk_heated},
            milkHeater()
    end.

mixer() ->
    receive
        {CID, start_mixing} ->
            CID!{drink_is_mixing},
            timer:sleep(3000),
            CID!{drink_ready},
            mixer()
    end.

monitor(WaterState, MilkState, CoffeeState, CocoaState) ->
    receive
        {CID, water_heated} ->
            NewWaterState = 1,
            self()!{CID, check_status},
            monitor(NewWaterState, MilkState, CoffeeState, CocoaState);

        {CID, milk_heated} ->
            NewMilkState = 1,
            self()!{CID, check_status},
            monitor(WaterState, NewMilkState, CoffeeState, CocoaState);
            
        {CID, coffee_grinded} ->
            NewCoffeeState = 1,
            self()!{CID, check_status},
            monitor(WaterState, MilkState, NewCoffeeState, CocoaState);
            
        {CID, cocoa_poured} ->
            NewCocoaState = 1,
            self()!{CID, check_status},
            monitor(WaterState, MilkState, CoffeeState, NewCocoaState);
            
        {CID, check_status} ->
            case WaterState == 1 of
                false -> monitor(WaterState, MilkState, CoffeeState, CocoaState);
                true -> null
            end,

            case MilkState == 1 of
                false -> monitor(WaterState, MilkState, CoffeeState, CocoaState);
                true -> null
            end,

            case CoffeeState == 1 of
                false -> monitor(WaterState, MilkState, CoffeeState, CocoaState);
                true -> null
            end,

            case CocoaState == 1 of
                false -> monitor(WaterState, MilkState, CoffeeState, CocoaState);
                true -> null
            end,

            CID!{drink_ready_to_mix},
            monitor(0,0,0,0)
    end.



start() ->
    DisplayID = spawn(?MODULE, display, []),
    ProductsID = spawn(?MODULE, products, [productsAtStart()]),
    PaymentID = spawn(?MODULE, paymentTerminal, []),
    MonitorID = spawn(?MODULE, monitor, [0,0,0,0]),

    WaterHeaterID = spawn(?MODULE, waterHeater, []),
    MilkHeaterID = spawn(?MODULE, milkHeater, []),
    CoffeeGrinderID = spawn(?MODULE, coffeeGrinder, []),
    MixerID = spawn(?MODULE, mixer, []),

    ComputerID = spawn(?MODULE, computer, [DisplayID, ProductsID, PaymentID, MonitorID, {WaterHeaterID, MilkHeaterID, CoffeeGrinderID, MixerID}]),
    ComputerID!{initialize}.


% Funkcje pomocnicze z panelu
print({gotoxy,X,Y}) ->
   io:format("\e[~p;~pH",[Y,X]);
print({clear}) ->
   io:format("\ec",[]).

