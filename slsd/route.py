from log import Log
from aux import synchronized_method



class ERoutingResult:
	STOP_OK   = 0
	STOP_FAIL = 1
	NEXT      = 2


class Router:
	def __init__(self):
		self.__routes = []
		self.__sorted = False
	
	
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
		
		## Now we can safely add the route
		self.__routes.append(route)
		self.__sorted = False
	
	
	def removeRoute(self, route):
		self.__routes.remove(route)
	
	
	def addRoutes(self, routes):
		for (scanners, domain, port) in routes:
			self.addRoute(Route(scanners, domain, port))
	
	
	def __assureSorted(self):
		if not self.__sorted:
			self.__routes = sorted(
				self.__routes,
				reverse = True,
				key = lambda x: x.priority()
			)
			
			self.__sorted = True
	
	
	@synchronized_method
	def scanned(self, scan):
		self.__assureSorted()
		
		for route in self.__routes:
			if route.match(scan):
				ret = route.call(scan)
				
				if ret == ERoutingResult.STOP_OK:
					pass
				
				elif ret == ERoutingResult.STOP_FAIL:
					Log.log(Log.LVL_WARN, Log.SRV_ROUTE, "The port failed")
				
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
				
				return
		
		Log.log(Log.LVL_NOTICE, Log.SRV_ROUTE, "No route for %s" % scan)



class Route:
	##
	# A route maps scans to ports. The routing is based on the attributes of a
	# scan. There can be only one matching route (so a port can't influence the
	# routing by asking the router to pick the next matching route). A route
	# defines a partition for each scan attribute.
	# 
	# @param scanners A list of scanners which a scan has to be in to match the
	#                 route.
	#
	# @param domain A String the scanned code has to be prefix with to match the
	#               route. "" is the default domain, if no other matches.
	#
	# @param port A callback function to be called with the remaining code
	#             (truncated by the domain) if the rout matches.
	def __init__(self, scanners, domain, port):
		self.__scanners = frozenset(scanners)
		self.__domain   = domain
		self.__port     = port
	
	
	def priority(self):
		return len(self.__domain)
	
	
	##
	# Tests if the two routes would match the same scan. In other words it tests
	# whether the cross products of the defined partitions (of scan attributes)
	# overlap.
	# 
	# @param The other route to match against.
	# 
	# @return True if this route and the parameter's route would match the same
	#         scan, otherwise False.
	def isDuplicate(self, route):
		return (
			( # The routes have overlapping scanners
				len(route.__scanners & self.__scanners) > 0
			) and
			( # The routes have overlapping domains
				(
					route.__domain == "" and self.__domain == ""
				) or
				(
					(
						route.__domain.startswith(self.__domain) or 
						self.__domain.startswith(route.__domain)
					) and
					route.__domain != "" and
					self.__domain  != ""
				)
			)
		)
	
	
	##
	# Test if the scan matches this route.
	def match(self, scan):
		return (
			scan.getScanner() in self.__scanners and
			scan.getCode().startswith(self.__domain)
		)
	
	
	##
	# Call the port with the scanned code truncated by the domain.
	# 
	# @param The scan containing the code for the port.
	# 
	# @return The return value of the port which has to be from ERoutingResult.
	def call(self, scan):
		return self.__port(scan.getCode()[len(self.__domain):])

