#' Resamples Hauls.
#' 
#' This function resamples Hauls with replacement by altering the LengthDistributionWeight.
#' 
#' @param LengthDistributionData The \code{\link[RstoxBase]{LengthDistributionData}} to resample Hauls in.
#' 
#' @export
#' 
resampleHauls <- function(LengthDistributionData, seed, by = "Stratum") {
    
    uniqueStrata <- unique(LengthDistributionData$Stratum)
    seedTable <- data.table::data.table(
        Stratum = uniqueStrata, 
        seed = getSeedVector(seed, size = length(uniqueStrata))
    )
    LengthDistributionData <- merge(LengthDistributionData, seedTable, by = by)
    
    LengthDistributionData[, WeightedCount := resampleHaulsOne(.SD, seed = seed[1]), by = by]
    
    return(LengthDistributionData)
}



# Function to resample Hauls of one subset of the data:
resampleHaulsOne <- function(subData, seed) {
    
    # Get unique Hauls:
    uniqueHauls <- unique(subData$Haul)
    uniqueHaulsResampled <- sampleSorted(uniqueHauls, seed = seed, replace = TRUE)
    
    # Tabulate the sampled Hauls:
    uniqueHaulsResampledTable <- table(uniqueHaulsResampled)
    uniqueHaulsResampledTable <- data.table::data.table(
        Haul = names(uniqueHaulsResampledTable), 
        resampleHaulsCount = uniqueHaulsResampledTable
    )
    
    # Merge the resampled counts into the data:
    subData <- merge(subData, uniqueHaulsResampledTable, by = "Haul", allow.cartesian = TRUE)
    # Insert the new count into WeightedCount (with NAs replaced by 0):
    WeightedCount <- subData[, ifelse(is.na(subData$resampleHaulsCount), 0, subData$resampleHaulsCount)]
    
    return(WeightedCount)
}




#### Tools to perform resampling: ####

# Function to sample after sorting:
sampleSorted <- function(x, size, seed, replace = TRUE, sorted = TRUE){
    # If not given, get the size of the sample as the length of the vector:
    lx <- length(x)
    if(missing(size)){
        size <- lx
    }
    if(sorted){
        x <- sort(x)
    }
    # Sample:
    set.seed(seed)
    x[sample.int(lx, size = size, replace = replace)]
    return(x)
}

getSeedVector <- function(seed, size = 1) {
    set.seed(seed)
    sample(getSequenceToSampleFrom(), size, replace=FALSE)
}

getSequenceToSampleFrom <- function(){
    size <- 1e7
    seq_len(size)
}