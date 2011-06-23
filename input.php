<?php

$greet = function($name)
{
    echo $name;
};

$greet :: TyFun [TyString] TyString
$greet ("hello world");

?>

$id :: forall a. a -> a
$id = function ($x)
{
    return $x;
}

$plus :: TyInt -> TyInt -> TyInt
$plus = function ($x, $y)
{
    return $x + $y;
}

 
Steps:

1. Roep de solver aan met mapping met type variables. Eg: plus met een mapping [$x :-> a, $y :-> b]
2. Het resulttype is te vinden in de resultaat ValueMap onder de key ReturnValue (Als er geen ReturnValue in de ValueMap zit wordt het return type TyVoid)
3. Verzamel alle expected constraints voor de parameters. Eg. voor plus geeft dit $x :-> TyInt en $y :-> TyInt
4. Update het resultaat type eventueel
5. Wanneer de parameters nog type variabelen bevatten worden deze opgenomen in een type scheme

$map = (a -> b) -> TyArray a -> TyArray b
$map = function ($func, $xs)
{
    if ($xs) { 
    
    }
    
    for ($i = 0; $i < count ($xs); $i++) {
        $xs[$i] = $func ($xs[$i]);
    }
    
    return $xs;
}

$apply :: (a -> b) -> a -> b
$apply = function ($func, $expr)
{
    return $func ($expr);
}


Analyse [return $func ($expr)]

1. Eerst de closure aanroep, het resultaat type is gelijk aan de expecttype voor de expressie [$func ($expr)] (In dit geval onbekend -> type variable)
       (params.mapping = updateMapping ... @lhs.mapping)
2. Nu de parameter [$expr]

generate :: Node -> Constraints -> Mapping -> Mapping
generate (Return e)                constraints mapping = generate e S.empty mapping
generate (Call (Closure n) params) constraints mapping = case lookup n mapping of 
                                                             Just (TyFun tyParams tyResult) -> (tyParams, tyResult)
                                                             Nothing                        -> let tyParams = [TyVar a, TyVar b, ...]
                                                                                                   tyResult = TyFun tyParams (resolve constraints @label)
                                                                                               in (tyParams, tyResult)
                                                         in M.fold join M.empty . M.map (\param -> generate param (toConstraint tyVar) 
generate (Variable n)              constraints mapping = updateWith join n (resolve constraints @label) mapping
generate (Plus lef right)          constraints mapping = generate left (@label :==: TyInt) mapping `M.unionWith join` generate right (@label :==: TyInt)


$inc :: Int -> Int
$inc = function ($x)
{
    $foo = $x;
    
    $foo = $foo + 1;
    
    return $foo;
}

