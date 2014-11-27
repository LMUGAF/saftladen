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
	if(is_array($response->body)) {
		array_walk_recursive(
			$response->body,
			function (&$item, $key) {
				if(is_string($item)) $item = utf8_encode($item);
			}
		);
	}
	
	$response->body = json_encode($response->body);
/*
	var_dump(mb_detect_encoding($response->body['name']));
	$response->body = json_encode($response->body, JSON_UNESCAPED_UNICODE);
	var_dump($response->body);*/
}
$response->contentType = $request->contentType;

$response->output();
