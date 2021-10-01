val str = CalculatedValue.str("foo")
  val dbl = CalculatedValue.dbl(12.2)

  str + str
  dbl - dbl 

  sealed trait SupportsPlus[A] {
    def add(l: A, r: A): A
  }
  object SupportsPlus {
    def apply[A](f: (A, A) => A): SupportsPlus[A] = 
      new SupportsPlus[A] {
        def add(l: A, r: A): A = f(l, r)
      }
    implicit val IntSupportsPlus: SupportsPlus[Int] = SupportsPlus((l, r) => l + r)
    implicit val DoubleSupportsPlus: SupportsPlus[Double] = SupportsPlus((l, r) => l + r)
    implicit val StringSupportsPlus: SupportsPlus[String] = SupportsPlus((l, r) => l + r)
  }

  /**
   * EXERCISE 1
   *
   * Design a data type called `CalculatedValue`, which represents a `Value` that is dynamically
   * computed from a `Spreadsheet`.
   */
  final case class CalculatedValue[Out](calculate: Spreadsheet => Out) { self =>

    /**
     * EXERCISE 2
     *
     * Add an operator that returns a new `CalculatedValue` that is the negated version of this one.
     */
    def unary_- (implicit ev: Out <:< Double): CalculatedValue[Double] = 
      CalculatedValue { spreadsheet =>
        -self.widen[Double].calculate(spreadsheet)
      }

    /**
     * EXERCISE 3
     *
     * Add a binary operator `+` that returns a new `CalculatedValue` that is the sum of the two
     * calculated values.
     */
    def +(that: CalculatedValue[Out])(implicit supportsPlus: SupportsPlus[Out]): CalculatedValue[Out] = 
      CalculatedValue { spreadsheet =>
        supportsPlus.add(self.calculate(spreadsheet), that.calculate(spreadsheet))
      }

    /**
     * EXERCISE 4
     *
     * Add a binary operator `-` that returns a new `CalculatedValue` that is the difere;nce of the
     * two calculated values.
     */
    def -(that: CalculatedValue[Out])(implicit ev: Out <:< Double): CalculatedValue[Double] = 
      CalculatedValue { spreadsheet =>
        val l = self.widen[Double]
        val r = that.widen[Double]

        l.calculate(spreadsheet) - r.calculate(spreadsheet)
      }

    def widen[Out2](implicit ev: Out <:< Out2): CalculatedValue[Out2] = 
      CalculatedValue(s => ev(self.calculate(s)))
  }
  object CalculatedValue {

    /**
     * EXERCISE 5
     *
     * Add a constructor that makes an `CalculatedValue` from a `Value`.
     */
    def str(contents: String): CalculatedValue[String] = CalculatedValue(_ => contents)

    def dbl(contents: Double): CalculatedValue[Double] = CalculatedValue(_ => contents)

    /**
     * EXERCISE 6
     *
     * Add a constructor that provides access to the value of the
     * specified cell, identified by col/row.
     */
    def at(col: Int, row: Int): CalculatedValue[_] = ???
  }