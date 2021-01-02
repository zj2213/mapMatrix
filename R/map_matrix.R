#' Print numbers in circle
#'
#' This function helps to create a n*m matrix
#' with ascending numbers printed in circle.
#'
#' @export
#' @param max_row Maximum rows of the matrix
#' @param max_col Maximum columns of the matrix
#' @return A matrix with ascending numbers in circle
#' @examples
#' circle_matrix(4,4)
#' circle_matrix(6,5)


circle_matrix <- function(max_row, max_col){
  map <- matrix("NA",nrow=max_row,ncol=max_col)
  i <- 1;j <- 1;i.start <- 1;j.start <- 1;run <- 1
  while(i <= max_row & j <= max_col){
    while(j <= max_col){
      if(map[i,j] == "NA"){ #horizontal right
        map[i,j] <- run
        run <- run + 1
        j <- j + 1
        }
      } # i=1, j=11, run=11
    j <- j -1
    while(i <= max_row){
      if(map[i,j] != "NA") i<-i+1
      else if(map[i,j] == "NA"){ #vertical down
        map[i,j] <- run
        run <- run + 1
        i <- i + 1
      }
      }
    i <- i-1
    while(j <= max_col & j >= j.start){
      if(map[i,j] != "NA") j<-j-1
      else if(map[i,j] == "NA"){ #horizontal left
        map[i,j] <- run
        run <- run + 1
        j <- j - 1
        }
      }
    i<-i-1
    j <- j.start
    while(i <= max_row & i > i.start){
      if(map[i,j] == "NA"){ #vertical up
        map[i,j] <- run
        run <- run + 1
        i <- i - 1
        }
      }
    i <- i+1
    j <- j+1
    j.start <- j.start + 1
    i.start <- i.start + 1
    max_col <- max_col - 1
    max_row <- max_row - 1
  }
  return(map)
}





