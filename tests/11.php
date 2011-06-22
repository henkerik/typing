<?php

function foo ($x)
{
    return $x + $x;
}

$a = 1;
$b = foo ($a);

check ($a, "[TyInt]");
check ($b, "[TyInt]");

?>