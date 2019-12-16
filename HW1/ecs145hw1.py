def check_indicator(x, ncol):
    '''
    Goal:
      Check the variable is indicative or not
    Input Parameters:
      x -- the whole matrix
      ncol -- the number of columns
    Output:
      a list of column indexes of indicator variables
    '''
    # indc_set: all column indexes
    indc_set = []
    # get x transpose
    xt = map(list, zip(*x))

    # get unique value for each column
    xt_set = map(set, xt)
    # get indicator indexes
    for index, col in enumerate(xt_set):
        if col == set([0, 1]) or col == set([0]) or col == set([1]):
            indc_set.append(index)
    return indc_set


def converttobase(num, base):
    '''
    Goal:
      convert decimal number to specific base system
    Input Parameters:
      num -- the number in decimal format
      base -- the base need to be converted to
    Output:
      a list that stores the result of number after converting
    '''
    l= []
    while num != 0:
        remainder = num % base
        num = num // base
        # store the digits in a list
        l = [remainder] + l
    return l


def exponent(variables, order):
    '''
    Goal:
      get all possible combination of exponents
    Input Parameters:
      variables -- number of variables
      order -- the maximum degree
    Output:
      a list of all possible combination of exponents
    Method:
      Similar to Hex Conveter
      e.x: When degree is 3 and number of variables is 4,
           the maximum number satisfied our requirements is 3000(Quaternary),
           which is 192 (Decimal). Then we do iterations from 1 to 192 (Decimal),
           and convert them back to Quaternary with a constraint (sum <= degree)
           to get all component combinations.
    '''
    result = []
    base = order + 1
    # get total iteration
    maxint = order * (base**(variables - 1))
    for i in range(1,maxint + 1):
        # convert number in decimal to the specified  base system
        l = converttobase(i, base)
        # turn into desired format
        length = len(l)
        expo = (variables - length) * [0] + l
        # filter out unwanted results
        if sum(expo) <= order:
            result.append(expo)
    return result



def polygen(x, deg):
    '''
    Goal:
      Create the matrix for later LSE derivation
    Input Parameters:
      x -- original matrix
      deg -- the degree of the polynomial
    Output:
      The matrix of polynomial data
    '''
    # create an empty list for final output
    output = []
    # get the number of rows and columns
    nrow = len(x)
    ncol = len(x[0])

    #  select out the column indexes of non-indicator & indicator variable list
    indc_set = check_indicator(x, ncol)

    #  get the exponential part
    exp = exponent(ncol, deg)

    # iteration on rows
    for i in range(nrow):
        # make a copy of x[i] without changing x for below iterations
        current = x[i][:]

        # create the output of each row
        sub_output = []

        # add the polynomial part by exponents
        for j in exp:
            if sum(map(lambda x: j[x] >= 2, indc_set)) == 0:
                sub_output += [reduce(lambda x, y: x*y,
                                      map(lambda u, v: u**v, current, j))]

        output.append(sub_output)

    return output