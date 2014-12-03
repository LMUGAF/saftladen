<?php

namespace Slrest;

use Tonic\Response;
use PDO;


/**
 * This class defines an example resource that is wired into the URI /example
 * @uri /users/id/:id
 * @uri /users/name/:name
 */
class UserResource extends User {
	private function selector(&$values) {
		foreach(array("id", "name") as $key) {
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
		
		$st = $this->db->prepare("SELECT * FROM `user` WHERE ".$this->selector($values));
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
		$st = $this->db->prepare("UPDATE `user` SET ".$this->pdoSet($values)." WHERE ".$this->selector($values));
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
		return new Response(Response::NOCONTENT);
	}
}