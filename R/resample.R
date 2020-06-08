#' Resamples Hauls.
#' 
#' This function resamples Hauls with replacement by altering the LengthDistributionWeight.
#' 
#' @param LengthDistributionData The \code{\link[RstoxBase]{LengthDistributionData}} to resample Hauls in.
#' 
#' @export
#' 
resampleHauls <- function(LengthDistributionData, seed, by = "Stratum") {
    
    # Get the unique strata:
    uniqueStrata <- unique(LengthDistributionData$Stratum)
    
    # Build a table of Stratum and seed and merge with the LengthDistributionData:
    seedTable <- data.table::data.table(
        Stratum = uniqueStrata, 
        seed = getSeedVector(seed, size = length(uniqueStrata))
    )
    LengthDistributionData <- merge(LengthDistributionData, seedTable, by = by)
    
    browser()
    
    # Resample the Hauls:
    LengthDistributionData[, LengthDistributionWeight := resampleOne(.SD, seed = seed[1], var = "Haul"), by = by]
    return(LengthDistributionData)
}

#' Resamples EDSUs
#' 
#' This function resamples Hauls with replacement by altering the LengthDistributionWeight.
#' 
#' @param NASCData The \code{\link[RstoxBase]{NASCData}} to resample EDSUs in.
#' 
#' @export
#' 
resampleEDSUs <- function(NASCData, seed, by = "Stratum") {
    
    # Get the unique strata:
    uniqueStrata <- unique(NASCData$Stratum)
    
    # Build a table of Stratum and seed and merge with the NASCData:
    seedTable <- data.table::data.table(
        Stratum = uniqueStrata, 
        seed = getSeedVector(seed, size = length(uniqueStrata))
    )
    NASCData <- merge(NASCData, seedTable, by = by)
    
    # Resample the Hauls:
    NASCData[, NASCData := resampleOne(.SD, seed = seed[1], var = "EDSU"), by = by]
    return(NASCData)
}

#resampleHauls <- function(LengthDistributionData, seed, by = "Stratum") {
#    # Get the unique strata:
#    uniqueStrata <- unique(LengthDistributionData$Stratum)
#    # Build a table of Stratum and seed and merge with the LengthDistributionData:
#    seedTable <- data.table::data.table(
#        Stratum = uniqueStrata, 
#        seed = getSeedVector(seed, size = length(uniqueStrata))
#    )
#    LengthDistributionData <- merge(LengthDistributionData, seedTable, by = by)
#    
#    # Resample the Hauls:
#    LengthDistributionData[, LengthDistributionData := resampleHaulsOne(.SD, seed = seed[1]), by = by]
#    return(LengthDistributionData)
#}







# Function to resample Hauls of one subset of the data:
resampleOne <- function(subData, seed, var = c("Haul", "EDSU")) {
    
    # Get the vavriable to resample:
    var <- match.arg(var)
    
    #count <- data.table::data.table(
    #    unique(subData[[var]])
    #)
    #setnames(count, var)
    
    # Get unique values:
    resampled <- sampleSorted(unique(subData[[var]]), seed = seed, replace = TRUE)
    # Tabulate the sampled Hauls:
    resampleTable <- data.table::as.data.table(table(resampled))
    setnames(resampleTable, c("resampled","N"), c(var, "resampledCountWithUniqueName"))

    # Merge the resampled counts into the data:
    count <- merge(subData, resampleTable, by = var, all = TRUE)
    
    # Insert the new count into WeightedCount (with NAs replaced by 0):
    count <- subData[, ifelse(
        is.na(count$resampledCountWithUniqueName), 
        0, 
        count$resampledCountWithUniqueName
    )]
    
    print(sum(count))
    
    return(count)
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
    sampled <- x[sample.int(lx, size = size, replace = replace)]
    return(sampled)
}

getSeedVector <- function(seed, size = 1) {
    set.seed(seed)
    sample(getSequenceToSampleFrom(), size, replace=FALSE)
}

getSequenceToSampleFrom <- function(){
    size <- 1e7
    seq_len(size)
}

