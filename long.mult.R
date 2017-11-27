long.mult <- function( x, y ){

  # Computes the product of the given factors.
  #
  # Args:
  #   x: One of two factors to be multiplied.
  #   y: The other factor.
  #
  #   x and y are numeric vectors
  #   each element of x and y is a non-negative integer between 000 and 999, inclusive.
  #
  # Returns:
  #   The product of x and y.

  # ---------------------------------------------
  # Auxiliary functions
  # ---------------------------------------------

  as_digit_vector <- function( numeric_vector ){

    # Auxiliary function converts a vector of digits ( i.e., 123456 )
    # into a vector of integers, each in the range 0 to 9
    #
    # Args:
    #   numeric_vector: for example, the number 123456
    #
    # Returns:
    #   A vector of integers, each in the range 0 to 9

    num_vec <- numeric_vector[ !is.na( numeric_vector )]

    char_vec  <- paste( sprintf( '%03d', num_vec ), collapse = '' )

    unlist( lapply(
        1:nchar( char_vec )
      , function( i ) as.numeric( substr( char_vec, i, i ))
    ))

  }

  num_digits <- function( f ){

    # Auxiliary function returns the number of digits in
    # a number, excluding leading zeroes.

    # Args:
    #   numeric_vector: for example, the number 123456
    #
    # Returns:
    #   A vector of integers, each in the range 0 to 9

    # Convert input to vector of digits
    digit_vec <- as_digit_vector( f )
  
    # Remove leading zeros
    while( digit_vec[ 1 ] == 0 ) digit_vec <- digit_vec[ 2:length( digit_vec ) ]
  
    # Return the length of the result
    length( digit_vec )
  }

  # ---------------------------------------------
  # The main function that does the work
  # ---------------------------------------------

  cached_function <- function(){

    # Auxiliary function multiplies two integers of arbitrary length,
    # remembering intermediate results between calls.
    #
    # Args:
    #   no explicit arguments
    #
    # Returns:
    #   The product of two integers.

    answer  <- NULL
    factors <- list()
    number_of_intermediate_products <- NULL
    answer_width <- NULL
    intermediate_products <- list()
    justified_table <- matrix()
    index1  <- 1       # 1: least-significant digit
    index2  <- 1       # n: most-significant digit

    # Internal functions --------------
    digit_operation <- function( d1, d2, op, carry_in = 0 ){

      # Auxiliary function adds or multiplies two digits,
      # carrying excess values to the next column.
      #
      # Args:
      #   d1, d2  : two digits to use as operands
      #   op      : the operator to apply
      #   carry_in: excess values to add to the current operation,
      #             carried from the previous operation; defaults to zero
      #
      # Returns:
      #   The sum or product of the inputs, plus carried value.

      switch( op
        , multiply = result <- d1 * d2 + carry_in
        , add      = result <- d1 + d2 + carry_in
      ) # end switch

      carry  <- ( result - result %% 10 ) / 10
      carry <- ifelse( is.numeric( carry ), carry, 0 )

      digit <- result %% 10
      digit <- ifelse( is.numeric( digit ), digit, 0 )

      list( carry = carry, digit = digit )    

    } # end function

    # Convenience wrappers:
    multiply_digits <- function( d1, d2, carry_in = 0 ){
      digit_operation( d1, d2, op = 'multiply', carry_in = carry_in )
    }

    add_digits <- function( d1, d2, carry_in = 0 ){
      digit_operation( d1, d2, op = 'add', carry_in = carry_in )
    }

    is_in_range <- function( double_vec, range_min = 000, range_max = 999 ){

      # Auxiliary determines whether all input value are within the desired range
      #
      # Args:
      #   double_vec : a vector of doubles
      #   range_min : defaults to 000.
      #   range_max : defaults to 999.
      #
      # Returns:
      #   The sum or product of the inputs, plus carried value.

      !any( unlist( lapply( double_vec
          , function( value ) ( value < 0 ) | ( value > 999 ) 
        )) )
    }

    # Given a digit vector and an index, N, return the Nth least significant digit from the vector
    digit_lookup <- function( digit_vector, N ) digit_vector[ 1 + length( digit_vector ) - N ]

    # Pad each intermediate product so it is as wide as the final answer
    pad_element <- function( obj, desired_length ){
      if( desired_length > length( obj ) ){
        pad_len <- desired_length - length( obj ) 
        new_obj <- rep( 0, desired_length )
        new_obj[ pad_len + 1:length( obj ) ] <- obj[ 1:length( obj ) ]
        return( new_obj )
      } else {
      return( obj )
      }
    }

  # Cached functions ----------------
  set_factors <- function( a, b ){
    if ( !is.numeric( a ) | !is.numeric( b ) ) stop( 'Factors must be numeric.' )
    if ( !is.vector(  a ) | !is.vector(  b ) ) stop( 'Factors must be vectors.' )
    if( !is_in_range( a ) | !is_in_range( b) ) stop( 'Each element of each input must be between 000 and 999, inclusive.')

    # Convert the input arguments from numeric vectors to character vectors,
    # i.e., c( 45 , 0  , 892, 012 )  -- becomes -->   '045000892012'
    x <- as_digit_vector( a )
    y <- as_digit_vector( b )
    number_of_intermediate_products <<- num_digits( b )
    answer_width <<- num_digits( a ) + num_digits( b )
    
    x <- pad_element( x, answer_width )
    y <- pad_element( y, answer_width )

    factors[[ 1 ]] <<- x
    factors[[ 2 ]] <<- y
    answer         <<- NULL  # Clear any previous answer when considering new factors
  }

  get_factors <- function() factors
  justify_products <- function(){

    padded <- matrix( data = 0, nrow = number_of_intermediate_products, ncol = answer_width )
    
    for( i in seq( nrow( padded ))){
      item <- intermediate_products[[ i ]]
      n <- length( item )
      for( j in seq( from = n, to = 1 )){
        padded[ i, answer_width - j + 1 ] <- item[ j ]
      }
    }

    # Return the padded table of intermediate products
    justified_table <<- padded
  }

  final_answer <- function(){

    as_clump <- function( v, i ){
      w <- length( v )
      result <- NULL
      if( w == 1 ) result <- v[ 1 ]
      if( w == 2 ) result <- 10*v[ 1 ] + v[ 2 ]
      if( w > 2 ){
        result <- 100*v[ i ] + 10*v[ i + 1 ] + v[ i + 2 ]
      }
      result
    }

    dig_vec <- vector( mode = 'numeric', length = answer_width )
    carry <- 0
    for ( i in seq( from = answer_width, to = 2 )){
      digit <- sum( justified_table[ , i ] ) + carry
      carry <- 0
      if( digit > 9 ){
        carry <- ( digit - digit %% 10 ) / 10
        digit <- digit %% 10
      }
      dig_vec[ i ] <- digit
    }
      dig_vec[ 1 ] <- sum( justified_table[ , 1 ] ) + carry # Final carry

    # Beginning with the least-significant digit, take 3
    # digits at a time and clump them into a double in the
    # range 000 to 999
    
    if( answer_width < 4 ){
      clumps <- as_clump( dig_vec, 1 )
    } else {
      indexes <- seq( from = length( dig_vec ) - 2, to = 1, by = -3 )
      clumps <- rev( purrr::map_dbl( indexes, ~as_clump( dig_vec, . )) )

      # Then, if there are any digits remaining, clump them
      n <- length( dig_vec) - 3*length( indexes)
      prepend <- switch( n + 1
       , NULL                            # remainder 0: no additional clump
       , dig_vec[ n ]                    # remainder 1: clump is 1st digit
       , 10*dig_vec[ 1 ] + dig_vec[ 2 ]  # remainder 2: clump is 10*1st digit + 2nd digit
      )

      if( !is.null( prepend )) clumps <- c( prepend, clumps )
    }

print( clumps )
    clumps

  }

  intermediate_product <- function( index2 ){
    # Multiply the current digits;
    # advance index1 until reaching msd, then reset index1 and advance index2
    # repeat until index2 reaches msd
    digit1 <- digit_lookup( factors[[ 1 ]], index1 )
    digit2 <- digit_lookup( factors[[ 2 ]], index2 )
    w <- length( factors[[ 1 ]] )

    carry = 0
      purrr::map( seq( w ), ~{
        digit1 <- digit_lookup( factors[[ 1 ]], . )
        result <- multiply_digits( digit1, digit2, carry_in = carry )
        carry <<- result$carry #result$digit %in% 0:9
        if( is.numeric( result$digit ) ) answer[ . ]     <<- result$digit
        if( is.numeric( result$carry ) ) answer[ . + 1 ] <<- result$carry
      }
    )
    intermediate_products[[ index2 ]] <<- c( rep( 0, index2 - 1 ), answer )    
    intermediate_products
  }

  list(
      set_factors = set_factors
    , get_factors = get_factors
    , justify_products = justify_products
    , final_answer = final_answer
    , intermediate_product = intermediate_product
  )

} # end cached_function

  # ---------------------------------------------
  # Perform the calculation
  # ---------------------------------------------
  
  # Instantiate the cached function to do the calculating and hold
  # intermediate results
  calculate <- cached_function()
  
  # Pass the factors to be multiplied
  calculate$set_factors( a = x, b = y )

  # Multiply each digit of the second factor by each digit of the
  # first factor, shifting the partial answer left one position each time
  this_sequence <- seq( num_digits( calculate$get_factors()[[ 2 ]] ) )
    purrr::map( this_sequence, ~calculate$intermediate_product( index2 = . ))
  
  justified_intermediate_products <- calculate$justify_products()

  # Combine the intermediate products into the final answer 
  final_ans <- calculate$final_answer()
} # end long.mult function


# -----------------------------------------------
# Usage examples:
# -----------------------------------------------
# long.mult( c( 82,456,342 ), c( 987,987 ) )
# long.mult( c( 987 ), c( 876 ) )
# long.mult( c( 823,456 ), c( 1,987 ) )
