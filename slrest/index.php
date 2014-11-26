<?php
// For debug purposes only!
error_reporting(E_ALL);
ini_set('display_errors', True);



require_once("conf.inc");
require_once("vendor/autoload.php");


$app = new Tonic\Application(array(
	'mount' => array('Slrest' => CONF_URL_PATH),
	'load' => 'Slrest/*Resource.php'
));

$request = new Tonic\Request();

// decode JSON data received from HTTP request
if($request->contentType == 'application/json') {
	$request->data = json_decode($request->data);
}
else {
	throw \Exception("Invalid Content-type");
}

$resource = $app->getResource($request);


$response = $resource->exec();

// encode output
if ($request->contentType == 'application/json') {
	$response->body = json_encode($response->body);
}
$response->contentType = $request->contentType;

$response->output();
