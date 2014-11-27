<?php

namespace Slrest;
use Tonic;
use Tonic\Response;
use PDO;


class ResourceBase extends Tonic\Resource {
	function __construct(
		Tonic\Application $app,
		Tonic\Request $request,
		array $urlParams,
		$tabName,
		$cols
	) {
		parent::__construct($app, $request, $urlParams);
		$this->tabName = $tabName;
		$this->cols    = $cols;
		$this->db = new PDO(CONF_DB_DSN, CONF_DB_USER, CONF_DB_PASS, array(
			// Trow real (and hence catchable) exceptions instead of PHP errors
			PDO::ATTR_ERRMODE            => PDO::ERRMODE_EXCEPTION,
			
			// Retrun rows as accociative arrays with the row name as the key
			PDO::ATTR_DEFAULT_FETCH_MODE => PDO::FETCH_ASSOC,
			
			// Return INTs as ints (requires php5-mysqlnd (native driver))
			PDO::ATTR_EMULATE_PREPARES   => false,
			
			// Return the number of found, not changed, rows on UPDATE queries
			PDO::MYSQL_ATTR_FOUND_ROWS   => true
		));
	}
	
	function pdoSet(&$values) {
		$set = '';
		$values = array();
		
		foreach($this->cols as $name => $col) {
			if(isset($this->request->data->$name)) {
				$values[$name] = $this->request->data->$name;
			}
			else if(isset($_POST[$name])) {
				$values[$name] = $_POST[$name];
			}
			else {
				continue;
			}
			
			$set .= "`{$name}` = :{$name}, ";
		}
		
		return substr($set, 0, -2); 
	}
	
	function selectVals() {
		if(isset($_GET["values"])) {
			$v = array_map(trim, explode($_GET["values"], ","));
		}
		else {
			$v = array_keys($this->cols);
		}
		
		return array_map(function($foo) { return "`$foo`"; }, $v)." FROM `{$this->tabName}` ";
	}
	
	function paginate($st) {
		$val = isset($_GET['offset']) ? (int)$_GET['offset'] : 0;
		$st->bindValue(':offset', $val, PDO::PARAM_INT);
		
		
		$val = isset($_GET['max']) ? (int)$_GET['max'] : 10;
		$val += 1;
		$st->bindValue(':max', $val, PDO::PARAM_INT);
		
		return $val;
	}
	
	function getAll() {
		$st = $this->db->prepare("SELECT * FROM `{$this->tabName}` LIMIT :offset, :max");
		$max = $this->paginate($st);
		$st->execute();
		if(($data['items'] = $st->fetchAll(PDO::FETCH_ASSOC)) === false) {
			return new Response(Response::BADREQUEST);
		}
		
		if(count($data['items']) == $max) {
			array_pop($data['items']);
			$data['hasmore'] = true;
		}
		else {
			$data['hasmore'] = false;
		}
		
		return new Response(Response::OK, $data);
	}
}

