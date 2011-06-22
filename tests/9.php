<?php

function id ($x)
{
    return $x;
}

$a = 1;
$b = id ($a);

$c = true;
$d = id ($c);

check ($a, "[TyInt]");
check ($b, "[TyInt]");

check ($c, "[TyBool]");
check ($d, "[TyBool]");

?>