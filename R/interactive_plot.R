## Since point picker is not working on filled.contour, this function will not work
# data(applejuice)
# n = 1
# drawEEM(applejuice, n)
# point <- locator(n = n)
# EX <- point$x
# EM <- point$y
# 
# tmp <- unfold(applejuice)[n, , drop = FALSE]
# par(mfrow = c(2,1))
# 
# # find nearest EX
# all_EX <- getEX(colnames(tmp))
# EX_nearest <- all_EX[which.min(abs(all_EX - EX))]
# 
# # find nearest EM
# all_EM <- getEM(colnames(tmp))
# EM_nearest <- all_EM[which.min(abs(all_EM - EM))]
# 
# drawSpec(tmp, EX = EX_nearest, ggplot = FALSE)
# drawSpec(tmp, EM = EM_nearest, ggplot = FALSE)

pickSpec <- function(EEM, n){
    drawEEM(EEM, n)
    cat("Please click on the plot to determine the point location.")
    point <- locator(n = n) # big problem now is that locator cannot be used with filled.contour. So locator() must be solved first
    EX <- point$x
    EM <- point$y
    
    data <- unfold(EEM)[n, , drop = FALSE]
    
    # find nearest EX
    all_EX <- getEX(colnames(data))
    EX_nearest <- all_EX[which.min(abs(all_EX - EX))]
    
    # find nearest EM
    all_EM <- getEM(colnames(data))
    EM_nearest <- all_EM[which.min(abs(all_EM - EM))]
    
    par(mfrow = c(2,1))
    drawSpec(data, EX = EX_nearest, ggplot = FALSE)
    drawSpec(data, EM = EM_nearest, ggplot = FALSE)
    
}


