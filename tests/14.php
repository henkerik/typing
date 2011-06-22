<?php

function fac ($n)
{
    if ($n == 0) {
        return 1;
    } else {
        return $n * fac ($n - 1);
    }
}

$a = fac (5);

check ($a, "[TyInt]");

?>