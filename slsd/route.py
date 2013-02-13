from log import Log
from aux import synchronized_method



class ERoutingResult:
	STOP_OK   = 0
	STOP_FAIL = 1
	NEXT      = 2


class Router:
	def __init__(self):
		self.__routes = []
	
	
	def addRoute(self, route):
		## Test if one of the existing routes already covers the new one
		for oldRoute in self.__routes:
			if oldRoute.isDuplicate(route):
				Log.log(
					Log.LVL_ERROR, Log.SRV_ROUTE,
					"Can't add a duplicated route, but trail and error "
					"routing is not implemented (by design)"
				)
				
				raise Exception()
		
		## Now we can savely add the route
		self.__routes.append(route)
	
	
	def removeRoute(self, route):
		self.__routes.remove(route)
	
	
	@synchronized_method
	def scanned(self, scan):
		found = False
		
		for route in self.__routes:
			if route.match(scan):
				found = True
				
				ret = route.call(scan)
				
				if ret == ERoutingResult.STOP_OK:
					pass
				
				elif ret == ERoutingResult.STOP_FAIL:
					Log.log(Log.LVL_WARN, Log.SRV_ROUTE, "The port faild")
				
				elif ret == ERoutingResult.NEXT:
					Log.log(
						Log.LVL_WARN, Log.SRV_ROUTE,
						"The port returned a \"next\", but trail and error "
						"routing is not implemented (by design)"
					)
				
				else:
					Log.log(
						Log.LVL_WARN, Log.SRV_ROUTE,
						"The port returned an invalid return code (%s)" % ret
					)
				
				break
		
		if not found:
			Log.log(
				Log.LVL_NOTICE, Log.SRV_ROUTE,
				"No route for %s" % scan
			)



class Route:
	def __init__(self, scanner, domain, port):
		self.__scanner = scanner
		self.__domain  = domain
		self.__port    = port
	
	
	## Test if the two routes would match the same scan
	def isDuplicate(self, route):
		return (
			(
				route.__scanner is None or 
				self.__scanner  is None or 
				route.__scanner == self.__scanner
			) and
			(
				route.__domain.startswith(self.__domain) or 
				self.__domain.startswith(route.__domain)
			)
		)
	
	
	def match(self, scan):
		return (
			scan.getCode().startswith(self.__domain) and
			(
				self.__scanner is None or
				self.__scanner == scan.getScanner() 
			)
		)
	
	
	## Call the port with the scanned code without the domain prefix
	def call(self, scan):
		return self.__port(scan.getCode()[len(self.__domain):])

