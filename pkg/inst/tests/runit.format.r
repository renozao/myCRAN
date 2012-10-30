# Unit tests for RNG formatting functions
# 
# Author: Renaud Gaujoux
###############################################################################

library(pkgmaker)

checkFun <- function(fn, name){
	
	function(x, ...){
		
		oldRNG <- RNGseed()
		if( !missing(x) ){
			d <- fn(x)
			obj <- getRNG(x)
			cl <- class(x)
		}else{
			d <- fn()
			obj <- getRNG()
			cl <- 'MISSING'
		}
		newRNG <- RNGseed()
		msg <- function(x, ...) paste(name, '-', cl, ':', x, '[', ..., ']')
		checkIdentical(oldRNG, newRNG, msg("does not change RNG", ...))
		
		#
		checkTrue( isString(d), msg("result is a character string", ...))
		checkIdentical(d, fn(obj), msg("digest is from the RNG setting", ...))
		
	}
}

test.RNGdigest <- function(){
	
	on.exit( RNGrecovery() )
	
	fn <- c('RNGdigest', 'RNGstr')
	sapply(fn, function(f){
		fn <- getFunction(f, where='package:rngtools')
		checker <- checkFun(fn, f)

		checker()
		checker(1234)
		checker(1:3, 'Valid seed')
		checker(2:3, 'Invalid seed')
		x <- list(10, rng=c(401L, 1L, 1L))
		checker(x, 'list with rng slot')
		
	})
	TRUE
}

checkRNGtype <- function(x, ..., expL=2L){
	
	fn <- RNGtype
	oldRNG <- getRNG()
	if( !missing(x) ){
		d <- fn(x)
		obj <- getRNG(x)
		cl <- str_c(class(x), '(', length(x), ')')
	}else{
		d <- fn()
		obj <- getRNG()
		cl <- 'MISSING'
	}
	newRNG <- getRNG()
	msg <- function(x, ...) paste(cl, ':', x, '[', ..., ']')
	checkIdentical(oldRNG, newRNG, msg("does not change RNG", ...))
	
	#
	checkTrue( is.character(d), msg("result is a character vector", ...) )
	checkIdentical( length(d), expL, msg("result has correct length (", expL, ")", ...) )
	
}

test.RNGtype <- function(){
	
	on.exit( RNGrecovery() )
	checker <- checkRNGtype
	
	checker()
	checker(1234)
	checker(1:3, 'Valid seed')
	x <- list(10, rng=c(401L, 1L, 1L))
	checker(x, 'list with rng slot')
	
	oldRNG <- getRNG()
	checkException(RNGtype(2:3), "Error with invalid seed")
	checkIdentical(oldRNG, getRNG(), "RNG still valid after error")
}
