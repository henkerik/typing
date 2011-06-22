<?php

function id ($x)
{
    return $x;
}

$a = 1;
$b = id ($a);

check ($a, "[TyInt]");
check ($b, "[TyInt]");


?>