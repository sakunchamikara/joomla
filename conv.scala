object conv {
	def main(args: Array[String]): Unit = {
		println("Select operation: ")
		println("\t1 - Decimal to Binary")
		println("\t2 - Decimal to Octal")
		println("\t3 - Decimal to Hex")
		println("\t4 - Binary to Decimal")
		println("\t5 - Binary to Octal")
		println("\t6 - Binary to Hex")
		println("\t7 - Octal to Decimal")
		println("\t8 - Octal to Binary")
		println("\t9 - Octal to Hex")
		println("\t10 - Hex to Decimal")
		println("\t11 - Hex to Binary")
		println("\t12 - Hex to Octal")

		printf(":")
		val opr:Int = scala.io.StdIn.readLine().toInt

		opr match {
			case 1 => {
				val n = scala.io.StdIn.readLine().toInt
				decToBinary(n)
			}

			/*case 2 => {
				val n = scala.io.StdIn.readLine().toInt
				decToOctal(n)
			}

			case 3 => {
				val n = scala.io.StdIn.readLine().toInt
				decToHex(n)
			}

			case 4 => {
				val n = scala.io.StdIn.readLine().toInt
				print(binaryToDec(n))
			}

			case 5 => {
				val n = scala.io.StdIn.readLine().toInt
				val m: Int = binaryToDec(n)
				println(decToOctal(m))
			}

			case 6 => {
				val n = scala.io.StdIn.readLine().toInt
				val m = binaryToDec(n)
				decToHex(m)
			}

			case 7 => {
				val n = scala.io.StdIn.readLine().toInt
				print(octalToDec(n))
			}

			case 8 => {
				val n = scala.io.StdIn.readLine().toInt
				val m = octalToDec(n)
				decToBinary(m)
			}

			case 9 => {
				val n = scala.io.StdIn.readLine().toInt
				val m = octalToDec(n)
				decToHex(m)
			}

			case 10 => {
				val n = scala.io.StdIn.readLine().toString
				print(hexToDec(n))
			}

			case 11 => {
				val n = scala.io.StdIn.readLine().toString
				val m = hexToDec(n)
				decToBinary(m)
			}

			case 12 => {
				val n = scala.io.StdIn.readLine().toString
				val m = hexToDec(n)
				decToOctal(m)
			}

			case _ => println("Invalid operation")*/

		}
	}

	def decToBinary(_n: Int) { 
        var binaryNum: Array[Int] = new Array[Int](1000)
        var n = _n
        var i: Int = 0;
        while (n > 0)  
        { 
            binaryNum(i) = n % 2
            n = n / 2
            i += 1
        } 
   
   		var j = 0
        for (j <- i - 1 to 0 by - 1) 
            print(binaryNum(j))
    }

    def decToOctal(_n: Int) { 
        var octalNum: Array[Int] = new Array[Int](1000)
        var n = _n
        var i: Int = 0;
        while (n > 0)  
        { 
            octalNum(i) = n % 8
            n = n / 8
            i += 1
        } 
   
   		var j = 0
        for (j <- i - 1 to 0 by - 1) 
            print(octalNum(j))
    } 

    def decToHex(_n: Int) { 
        var hexNum: Array[Int] = new Array[Int](1000)
        var n = _n
        var i: Int = 0;
        while (n > 0)  
        { 
            hexNum(i) = n % 16
            n = n / 16
            i += 1
        } 
   
   		var j = 0
        for (j <- i - 1 to 0 by - 1) 
            hexNum(j) match {
            	case 10 => print('A')
            	case 11 => print('B')
            	case 12 => print('C')
            	case 13 => print('D')
            	case 14 => print('E')
            	case 15 => print('F')
            	case _ => print(hexNum(j))
            }
    } 

    def binaryToDec(_n: Int): Int = {

            var dec:Int =  0
            var p:Int = 0
            var n:Int = _n
            
            while( n != 0)
            {
            	dec += (n%10)*(Math.pow(2, p).toInt)
                n = n / 10
                p += 1
            }

            return dec
    }

    def octalToDec(_n: Int): Int = {
            var dec:Int =  0
            var p:Int = 0
            var n:Int = _n
            
            while( n != 0)
            {
            	dec += (n%10)*(Math.pow(8, p).toInt)
                n = n / 10
                p += 1
            }

            return dec
    }

    def hexToDec(_s: String): Int = {
            var dec:Int =  0
            var p:Int = 0

            var s:String = _s
            var t: Int = s.length()
			var n = 0

            while(t != 0)
            {

            	s.charAt(t - 1) match {
            		case '0' => n = 0
            		case '1' => n = 1
            		case '2' => n = 2
            		case '3' => n = 3
            		case '4' => n = 4
            		case '5' => n = 5
            		case '6' => n = 6
            			
            		case '7' => n = 7
            		case '8' => n = 8
            		case '9' => n = 9
            		case 'A' => n = 10
            		case 'B' => n = 11
            		case 'C' => n = 12
            		case 'D' => n = 13
            		case 'E' => n = 14
            		case 'F' => n = 15
            	}

            	dec += (n)*(Math.pow(16, p).toInt)
                n = n / 10
                p += 1
                t -= 1
            }

            return dec
    }

}