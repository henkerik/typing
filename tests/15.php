<?php

$a = 1;
while (true) {
    $a[1] = $a;
}

// The expected type depends on the parameter k used by the widening function
check ($a, "[TyArray (TyArray (TyArray (TyAny)))]");

?>