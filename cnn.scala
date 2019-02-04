package package_name

object CNN {
    //Start Coding from here

   def activationLayerHelper(activationFunc : Double=>Double,Image_Row:List[Double]) : List[Double] = 
    {
    	Image_Row match {
    		case Nil => Nil 
    		case h::t => List(activationFunc(h)):::activationLayerHelper(activationFunc, t) 
    	}
    }

    def activationLayer(activationFunc : Double=>Double,Image:List[List[Double]]) : List[List[Double]] = {

    	Image match {
    		case Nil => Nil 
    		case h::t => List(activationLayerHelper(activationFunc,h)):::activationLayer(activationFunc, t) 
    	}

    }

    def dotProductHelper(row_1:List[Double],row_2:List[Double]):Double=
    {
    	row_1 match {
    		case Nil => 0
    		case h::t => (row_1.head * row_2.head ) + dotProductHelper(row_1.tail,row_2.tail)  
    	}
    }

    def dotProduct(matrix_1:List[List[Double]], matrix_2: List[List[Double]])  : Double = 
    {
    	
    	matrix_1 match {
    		case Nil => 0
    		case h::t => dotProductHelper(matrix_1.head,matrix_2.head) + dotProduct(matrix_1.tail,matrix_2.tail)  
    	}

    }


     def maxhelper(arr:List[Double],curr:Double= -2147483647):Double = 
    {
    
    	arr match {
    		case Nil => curr 
    		case h::t => { if(arr.head>curr) maxhelper(t,arr.head)
    						else maxhelper(t,curr)
    					}
    	}


    }

    def max(matrix:List[List[Double]],curr:Double= -2147483647):Double = 
    {
    	matrix match {
    		case Nil => curr
    		case h::t => {
    			val m = maxhelper(h,-2147483647)
    			if(m>curr) max(t,m)
    			else max(t,curr)
    	}

    	}

    }


    def maxpooler(xs: List[Double]): Double = {
    if(xs.tail.nonEmpty){
      val tl = maxpooler(xs.tail)
      if(tl > xs.head) tl
      else xs.head
    }else{
      xs.head
    }
  }

    def minhelper(arr:List[Double],curr:Double = 2147483647):Double = 
    {
    	
    	arr match {
    		case Nil => curr 
    		case h::t => { if(arr.head<curr) minhelper(t,arr.head)
    						else minhelper(t,curr)
    					}
    	}


    }

    def min(matrix:List[List[Double]],curr:Double=2147483647):Double = 
    {
    	matrix match {
    		case Nil => curr
    		case h::t => {
    			val m = minhelper(h,2147483647)
    			if(m<curr) min(t,m)
    			else min(t,curr)
    	}

    	}

    }

    def NormaliseHelper1D(d : Double,m:Double, Image_Row:List[Double]) : List[Int] = 
    {
    	
    	Image_Row match {
    		case Nil => Nil 
    		case h::t => List(((h-m)*255/d).round.toInt):::NormaliseHelper1D(d,m,t) 
    	}
    }

    def NormaliseHelper2D(d:Double,m:Double,Image:List[List[Double]]) : List[List[Int]] = {

    	Image match {
    		case Nil => Nil 
    		case h::t => List(NormaliseHelper1D(d,m,h)):::NormaliseHelper2D(d,m,t) 
    	}

    }


    def normalise(Image:List[List[Double]]):List[List[Int]]= {
    	val mini = min(Image)
    	val maxi = max(Image)
    	val d = maxi-mini
    	val mat = NormaliseHelper2D(d,mini,Image)
    	mat

    }

    def removeColumn1d(Row:List[Double], k:Int) : List[Double] = {
         (k,Row) match { 
            case(_,Nil)=> Nil
             case (0,Row) => Row
             case (k,h::t) => removeColumn1d(t,k-1) 
         }
    } 

    def removeColumn2d(Image:List[List[Double]], K:Int,done:Int) : List[List[Double]] = {
        if(K==done) {Nil}
        else (K,Image) match {
            case(k,Nil) => Nil 
            case(k,h::t) => List(removeColumn1d(h,k)):::removeColumn2d(t,k,done+1)
        }
    }


    def getSubList1d(Row:List[Double], k:Int) : List[Double] = {
        (k,Row) match {
             case(_,Nil) => Nil   
             case (1,h::t) => List(h)
             case (k,h::t) => List(h):::getSubList1d(t,k-1) 
         }
    }  

    def getSubList2d(Image:List[List[Double]], K:Int,done:Int): List[Double] ={
        if(K==done) Nil
        else
        (K,Image) match {
            case(k,Nil) => Nil 
            case(k,h::t) => getSubList1d(h,k):::getSubList2d(t,k,done+1)
        }
    } 

    def singlePooling (poolingFunc:List[Double]=>Double, Image:List[List[Double]] ,K:Int): List[Double]  = {
       //println ("image - " + Image)
        Image match {
            case Nil => Nil
            case h::t => {
                  h match {
                      case Nil => Nil
                      case _ =>  {val sublist = getSubList2d(Image,K,0)
                                    val remlist = removeColumn2d(Image,K,0)
                                    //println("extracted - " + sublist)
                                    //println("bacha hua - " + remlist)
                                    val ele = poolingFunc(sublist)
                                    List(ele):::singlePooling(poolingFunc,remlist,K)
                                }

                            }
                         }
        }
       


    }


    def removeRows(Image:List[List[Double]], K:Int) : List[List[Double]] = {
        (K,Image) match {
            case (_,Nil) => Nil
            case (1,h::t) => t
            case (_,h::t) => removeRows(t,K-1)
        }
    }

    def poolingLayer(poolingFunc:List[Double]=>Double, Image:List[List[Double]],K:Int) : List[List[Double]] =
    {
        Image match {
            case Nil => Nil
            case _ => { 
                List(singlePooling(poolingFunc,Image,K)):::poolingLayer(poolingFunc,removeRows(Image,K),K)
            }     
        }
    }

    def getElement(ImageRow:List[Double], ind:Int) : Double = 
    {
        (ImageRow,ind) match {
            case (_,0) => ImageRow.head
            case(_,_)=>getElement(ImageRow.tail,ind-1)    
        }
    }
    def getRow(ImageRow:List[Double],start:Int,sizeOfRow:Int,sizeOfImage:Int):List[Double] = {
        if(start+sizeOfRow>sizeOfImage) Nil
        else {
            (ImageRow,sizeOfRow) match {
                case (Nil,_) => Nil
                case(_,1)=> List(getElement(ImageRow,start)) 
                case (_,_) => List(getElement(ImageRow,start)):::getRow(ImageRow.tail,start,sizeOfRow-1,sizeOfImage-1)    
            }

        }
    }

    def getSubMatrix(Image:List[List[Double]], imageSize:List[Int], kernelSize:List[Int],row:Int,col:Int ,columnsLeft:Int): List[List[Double]] = {

        if(columnsLeft+row>imageSize.head) Nil
        else {
            (columnsLeft,Image) match {
                case(_,Nil)=>Nil
                case (1,_) => List(getRow(Image.head,col,kernelSize.tail.head,imageSize.tail.head))
                case (_,_) => List(getRow(Image.head,col,kernelSize.tail.head,imageSize.tail.head)):::getSubMatrix(Image.tail,imageSize,kernelSize,row+1,col,columnsLeft-1)
            }
        }


    }

    def convolute1d(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int],col:Int,row:Int):List[Double] =
     {
        if(col+kernelSize.tail.head>imageSize.tail.head) Nil
        else 
        {
        //    println("trying to extract from " + row + col + "size is " + imageSize)
         val sm = getSubMatrix(Image,imageSize,kernelSize,row,col,kernelSize.head)

       //  println("extracted matrix : " + sm)
         sm match {
             case Nil => Nil
             case _ => List(dotProduct(sm,Kernel)):::convolute1d(Image,Kernel,imageSize,kernelSize,col+1,row)   
         }
          }  
    }

    def convolute2d(Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int],col:Int,row:Int):List[List[Double]] =
    {
     // println("\n trying to call 2d on " + row+col)
        if(row+kernelSize.head>imageSize.head) Nil
        else List(convolute1d(Image,Kernel,imageSize,kernelSize,col,row)):::convolute2d(Image.tail,Kernel,imageSize,kernelSize,col,row+1)
        

    }
    def convolute (Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int], kernelSize:List[Int] ) : List[List[Double]] =
    {
       
       convolute2d(Image,Kernel,imageSize,kernelSize,0,0)
         
    }

   def mixedLayer( Image:List[List[Double]], Kernel:List[List[Double]], imageSize:List[Int],
                      kernelSize:List[Int], activationFunc: Double => Double, poolingFunc:List[Double]=>Double, K:Int): List[List[Double]] =
  {
    val convoluted = convolute(Image,Kernel,imageSize,kernelSize)
    val activated = activationLayer(activationFunc,convoluted)
    val pooled = poolingLayer(poolingFunc,activated,K)
    pooled
   }

   def avghelper(arr:List[Double],currsum:Double,n:Double):Double = 
   {
        arr match {
            case Nil => currsum/n
            case _ => avghelper(arr.tail,currsum+arr.head,n+1)
        }
   }

   def avg(arr:List[Double]):Double = {
    avghelper(arr,0,0)

   }

   def listAddition1d(l1:List[Double],l2:List[Double]):List[Double] = {
    l1 match {
        case Nil => Nil
        case _ => List(l1.head+l2.head):::listAddition1d(l1.tail,l2.tail)
    }
   }
  
   def listAddition2d(l1:List[List[Double]],l2:List[List[Double]]):List[List[Double]] = {

    l1 match {
        case Nil => Nil
        case _ => List(listAddition1d(l1.head,l2.head)):::listAddition2d(l1.tail,l2.tail)
    }

   }

  def listMult1d(l1:List[Double],w:Double):List[Double] = {
    l1 match {
        case Nil => Nil
        case _ => List(l1.head*w):::listMult1d(l1.tail,w)
    }
   }
  
   def listMult2d(l1:List[List[Double]],w:Double):List[List[Double]] = {

    l1 match {
        case Nil => Nil
        case _ => List(listMult1d(l1.head,w)):::listMult2d(l1.tail,w)
    }

   }



   def assembly(Image:List[List[Double]], imageSize:List[Int], w1:Double, w2:Double, b:Double, Kernel1:List[List[Double]],
    kernelSize1:List[Int], Kernel2:List[List[Double]], kernelSize2:List[Int], Kernel3:List[List[Double]], kernelSize3:List[Int], Size: Int) : List[List[Int]] = 
   {
        val temporaryoutput1 = mixedLayer(Image,Kernel1,imageSize,kernelSize1,(x:Double)=>if(x>0) x else 0,avg,Size)
        val temporaryoutput2 = mixedLayer(Image,Kernel2,imageSize,kernelSize2,(x:Double)=>if(x>0) x else 0,avg,Size)
        val w = (imageSize.tail.head - kernelSize1.tail.head + 1)/Size  
        val h = (imageSize.head - kernelSize1.head + 1)/Size 
        val w1matrix = listMult2d(temporaryoutput1,w1)
        val w2matrix = listMult2d(temporaryoutput2,w2)    
        val bmatrix = List.fill(h)(List.fill(w)(b))
        val p = listAddition2d(w1matrix,w2matrix)
        val temporaryoutput3 = listAddition2d(p,bmatrix)
        val temporaryoutput4 = mixedLayer(temporaryoutput3,Kernel3,List(h,w),kernelSize3,(x:Double)=>if(x>0) x else 0.5*x,maxpooler,Size)    
        normalise(temporaryoutput4)    
   }


 

}
