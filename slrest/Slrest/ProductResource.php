<?php

namespace Slrest;

use Tonic\Response;
use PDO;


/**
 * This class defines an example resource that is wired into the URI /example
 * @uri /products/id/:id
 * @uri /products/name/:name
 * @uri /products/ean/:ean
 */
class ProductResource extends Product {
	private function selector(&$values) {
		foreach(array("id", "name", "ean") as $key) {
			if($this->$key !== NULL) {
				$values["selector"] = $this->$key;
				return "`$key` = :selector";
			}
		}
	}
	
	
	/**
	 * @method GET
	 */
	function display() {
		$st = $this->db->prepare("SELECT * FROM `product` WHERE ".$this->selector($values));
		$st->execute($values);
		if(($data = $st->fetch(PDO::FETCH_ASSOC)) !== false) {
			return new Response(Response::OK, $data);
		}
		else {
			return new Response(Response::NOTFOUND);
		}
	}

	/**
	 * @method PUT
	 */
	function update() {
		$st = $this->db->prepare("UPDATE `product` SET ".$this->pdoSet($values)." WHERE ".$this->selector($values));
		if($st->execute($values)) {
			if($st->rowCount() === 1) {
				return new Response(Response::CREATED);
			}
			else {
				return new Response(Response::NOTFOUND);
			}
		}
		else {
			return new Response(Response::BADREQUEST);
		}
	}

	/**
	 * @todo: Implement by setting a deleted timestamp
	 * @method DELETE
	 */
	function remove() {
// 		$ds = $this->container['dataStore'];
// 		$ds->delete($this->id);
		return new Tonic\Response(Tonic\Response::NOCONTENT);
	}
}