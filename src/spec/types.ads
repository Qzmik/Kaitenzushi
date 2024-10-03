with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Types is

    type Chefs is (Lidl, Biedra);
    type SushiType is (A, B);
    type Orders is record
        foodType : SushiType;
        amount   : Positive;
    end record;
    type Customers is record
        name  : Unbounded_String;
        order : Orders;
    end record;
    type Sushi is record
        producer : Chefs;
        foodType : SushiType;
    end record;

    type Conveyor is array (0 .. 9) of Sushi;

end Types;
