# Runoff hydological groups table
# A group of soils having similar runoff potential under similar storm and cover conditions.
# Examples are A and A/D. (NSSH)

require(tidyr)

hydrogroup <- data.frame(matrix(data=c(61,64,68,71,73,76,80,83,81,84,88,91,84,87,91,94),
                            ncol = 4,nrow = 4,
                            dimnames = list(c(1,2,3,4),
                                            LETTERS[1:4])))


hydrogroup$"A/B" <- 0.5*(hydrogroup[,1]+hydrogroup[,1+1])
hydrogroup$"A/C" <- 0.5*(hydrogroup[,1]+hydrogroup[,1+2])
hydrogroup$"A/D" <- 0.5*(hydrogroup[,1]+hydrogroup[,1+3])

hydrogroup$"B/C" <- 0.5*(hydrogroup[,2]+hydrogroup[,2+1])
hydrogroup$"B/D" <- 0.5*(hydrogroup[,2]+hydrogroup[,2+2])

hydrogroup$"C/D" <- 0.5*(hydrogroup[,3]+hydrogroup[,3+1])


hydrogroup$slope_code <- as.numeric(row.names(hydrogroup)) # slope groups "0-2","2-5","5-10","10-100"

hydrogroup %>%
  gather(key="hydrogroup",value="CN2", - slope_code) -> hydrogroup

saveRDS(hydrogroup, "_data/hydrogroup.rds")
