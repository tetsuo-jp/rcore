<?php
$example = filter_input(INPUT_GET, "example", FILTER_VALIDATE_INT);
?>
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <title>R-CORE Playground</title>
</head>

<body>
<h1>R-CORE Playground</h1>

<form action="execute.php" method="post">
<h3>R-CORE code</h3>
<textarea name="prog" rows="20" cols="100">
<?php
if ($example == 1) {
	$filename = "skip.rcore";
} else if ($example == 2) {
	$filename = "move.rcore";
} else if ($example == 3) {
	$filename = "swap.rcore";
} else if ($example == 4) {
	$filename = "conditional.rcore";
} else if ($example == 5) {
	$filename = "conditional.rcore";
} else if ($example == 6) {
	$filename = "stack.rcore";
} else {
	$filename = "reverse.rcore";
}
$con = file_get_contents("examples/$filename");
echo($con);
 ?>
</textarea>
<h3>Input data</h3>
<textarea name="data" rows="10" cols="100">
<?php
if ($example == 1) {
    $data = "list_abc.rcore_val";
} else if ($example == 2) {
    $data = "list_abc.rcore_val";
} else if ($example == 3) {
    $data = "list_abc.rcore_val";
} else if ($example == 4) {
    $data = "false.rcore_val";
} else if ($example == 5) {
    $data = "true.rcore_val";
} else if ($example == 6) {
    $data = "nil.rcore_val";
} else {
    $data = "list_abc.rcore_val";
}
$con = file_get_contents("examples/$data");
echo($con);
?>
</textarea>
<h3>Options</h3>
<input type="checkbox" name="invert" value="1">Inversion
<!-- <input type="checkbox" name="p2d" value="1">Program2data -->
<input type="checkbox" name="exp" value="1">Expand macros
<h3>Execute</h3>
<input type="submit" value="Execute">
</form>

<h2>Sample programs and data</h2>
<ul>
 <li> <a href="index.php?example=1">skip</a>
 <li> <a href="index.php?example=2">move</a>
 <li> <a href="index.php?example=3">swap</a>
 <li> <a href="index.php?example=4">conditional 1</a>
 <li> <a href="index.php?example=5">conditional 2</a>
 <li> <a href="index.php?example=6">stack</a>
 <li> <a href="index.php?example=0">reverse</a>
</ul>

  </body>
</html>
