// Author: Zhengmao Zhang
object Qsort {
    // import scala.collection.mutable.PriorityQueue
    import java.util.PriorityQueue
    val MINSIZE = 10 // threshold for switching to bubble sort
    var arr:Array[Int] = _ // the array
    def createArray(N:Int) = {
        val rand = new scala.util.Random
        var i = 0;
        arr = new Array[Int](N)
        for( i <- 1 to N){
            var num = rand.nextInt()
            arr(i - 1) = rand.nextInt(N)
        }
    }
    def printArray(msg:String) = {
        print(msg)
        for ( itr <-arr ){
            print(itr + ", ")
        }
        println()
    }
    def bubbleSort(low:Int, high:Int) = {
        for(i<- low to high){
            for(j <- (i-1) to 0 by -1){
                if(arr(j)>arr(j+1)){
                    val temp=arr(j+1)
                    arr(j+1)=arr(j)
                    arr(j)=temp
                }
            }
        }
    }

    // – If (high - low < MINSIZE), use bubbleSort instead.
    // . Use a(high) as the pivot value to partition the array section and recursively quicksort the resulting
    // two subsections.
    // . Sort the array in-place, i.e. without using additional array storage.
    
    def quickSort(low:Int, high:Int): Unit = {
        if(high - low < MINSIZE){
            bubbleSort(low, high)
        }
        else if (high > low) {
            val p = partition(low, high)
            quickSort(low, p-1)
            quickSort(p+1, high)
        }
    }
    def swap(pos1: Int, pos2: Int): Unit = {
        val stash = arr(pos1)
        arr(pos1) = arr(pos2)
        arr(pos2) = stash
    }
    def partition(low: Int, hi: Int): Int = {
        val pivot = hi;
        var i = low;
        for (
        j <- low to hi
        if arr(j) < arr(pivot)
        ) {swap(i, j); i+=1}

        // Lastly, move pivot value to the dividing line at i
        swap(i, pivot);
        return i
    }
    def quickSortNR(low:Int, high:Int) = {
        // Space O(N) // TIME O(NlogN + N) = O(N log N)
        val pQueue = new PriorityQueue[Integer]()
        for ( itr <-arr ){
            pQueue.add(itr)
        }
        println(pQueue)
        val N = arr.length
        for( i <- 1 to N){
            arr(i - 1) = pQueue.poll()
        }
    }
    def verifyArray = {
        var prev = -1;
        var count = 0;
        for ( itr <-arr ){
            if(prev <= itr){
                count += 1
            }
            prev = itr
        }
        if(count == arr.length){
            println("Result verified!")
        }
        else{
            println("Result is NOT verified!")
        }
    }
    def main(argv: Array[String]) = {
        assert(argv.length > 0)
        val N = argv(0).toInt
        assert(N > 0)
        createArray(N)
        printArray("Initial array: ")
        bubbleSort(0, N-1)
        printArray("After BubbleSort: ")
        verifyArray
        createArray(N)
        printArray("Initial array: ")
        quickSort(0, N-1)
        printArray("After quickSort: ")
        verifyArray
        createArray(N)
        printArray("Initial array: ")
        quickSortNR(0, N-1)
        printArray("After quickSortNR: ")
        verifyArray
    }
}