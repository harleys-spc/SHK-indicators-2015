## shark-mitig-ws-2015_data-prep.r
## Reading in already extracted data from observer database
## Formatting for analysis of shark bycatch mitigation
## -------------------------------------------------------
## Author: Laura Tremblay-Boyer (l.boyer@fisheries.ubc.ca)
## Written on: April 21, 2015
## Time-stamp: <2015-04-25 14:27:31 lauratb>
options(stringsAsFactors=FALSE)
require(dplyr)
require(magrittr)

shkdir <- "C:\\Projects\\SHK_MITIGATION\\"

setlab <- scan(file=paste(shkdir,"Shk_mitigation field descriptions.txt",sep=""),
               what="character",skip=4,nlines=1)
catlab <- scan(file=paste(shkdir,"Shk_mitigation field descriptions.txt",sep=""),
               what="character",skip=11,nlines=1)


catch <- rbind(
read.csv(paste(shkdir,"ll_shark_catch_HWOB.csv",sep=""),header=F),
read.table(paste(shkdir,"ll_shark_catch_non_HWOB_21_04_2015.txt",sep=""),sep="\t",header=F))
names(catch) <- catlab

sets1 <- read.csv(paste(shkdir,"ll_shark_sets_HWOB.txt",sep=""),sep="\t",header=F)
names(sets1) <- setlab
sets2 <- read.table(paste(shkdir,"ll_shark_sets_non_HWOB_21_04_2015.txt",sep=""),sep="\t",header=F)
names(sets2) <- setlab

shkbait <- unlist(read.csv("sharkbait.csv")) # code for bait used for shark

##########################################################################
#################### Effort/set information ##############################
##########################################################################
## Data format
# Things that should be numeric
x <- c("set_start_time","set_end_time","haul_start_time",
       "lat1d","lon1d","hk_bt_flt","hook_set","hook_est",
       "lightsticks","bask_set","bask_observed","nbshark_lines")

sets1[,x] %<>% sapply(as.numeric)
sets2[,x] %<>% sapply(as.numeric)
sets <- rbind(sets1,sets2)
rm(sets1,sets2)

# remove duplicates
sets %<>% filter(!duplicated(l_set_id))

### Initial data cleaning

## Define fisheries:
# Add AS to M2
# Remove VU fishing in VU from M2
# Only CN and FM flagged vessels in fishery 1
# x <- sets[sets$fishery=='M1_FM',]
sets$fishery %<>% as.character
sets$fishery[sets$eez_code=="AS"] <- "M2_FV"    # then exclude Vanuatu ...
sets$fishery[sets$eez_code=="VU" & sets$flag != "FJ"] <- "MD_NA"    # then exclude Vanuatu ...
sets$fishery[sets$fishery=="M1_FM" & !(sets$flag %in% c('FM','CN'))] <- "MD_NA"    # Only CN and FM flagged vessels in fishery 1

# only data sets we are working with (chosen for Bruno and Carl because of fishery features)
# setdat <- sets[sets$fishery %in% c('M1_FM','M2_FV','M7_HD'),]

# extract year and month
sets$yy <- as.numeric(substring(sets$set_start_date,1,4))
sets$mm <- as.numeric(substring(sets$set_start_date,5,6))

# a simple relative soak time index (adjusting for overnight as needed)
sets$soak <-  with(sets, haul_start_time-set_start_time +
                         ifelse(haul_start_time > set_start_time, 0, 24))

# switch NA fields to 'no' for lightsticks and shark lines
sets$lightsticks[is.na(sets$lightsticks)] <- 0
sets$nbshark_lines[is.na(sets$nbshark_lines)] <- 0


# Was shark bait used at all on the set no-0 or yes-1
# Set to true if any of the bait variables match a value in shkbait
# (May 2015: less than 0.5% = 1)
baitcols <- sprintf("bait%s_sp_code",1:5) # columns with bait info
setdat$sharkbait <- as.numeric(apply(apply(setdat[,baitcols],2, "%in%", shkbait),1,any))

# filtering out missing or inconsistent data
# get rid of data with NA's in critical fields  - hk_bt_flt and hook_est
sets <- sets[!(is.na(sets$hk_bt_flt) | is.na(sets$hook_set) | is.na(sets$hook_est) |
                   is.na(sets$lon1d)),]
sets <- sets[sets$sharktarget=="N",] # no shark targeting
# get rid of records where hook_set!=hook_est
sets <- sets[sets$hook_set==sets$hook_est,]
# less than 40 hbf and at least five and at least 1000 hooks set
sets <- sets[sets$hook_set >= 1000 & sets$hk_bt_flt <= 40 & sets$hk_bt_flt >= 5,]

# switch negative values for longitude data
sets$lon1d %<>% "+"(ifelse(sets$lon1d<0, 360, 0))


###################################################################################  NOW GROOM THE CATCH DATA
# Things that should be numeric
x <- c("catch_time","hk_bt_flt","hook_no")
catch[,x] %<>% sapply(as.numeric)

# individual species to include
sp <- c('FAL','OCS','BSH') # silky, oceanic whitetip
THR <- c('THR','BTH','PTH','ALV')
MAK <- c('MAK','SMA','LMA')
HHD <- c('SPN','SPZ','SPL','SPK','EUB')

# make my own categories using sp_category
catch$sp_category[catch$sp_code %in% sp] <- catch$sp_code[catch$sp_code %in% sp]
catch$sp_category[catch$sp_code %in% THR] <- "THR"
catch$sp_category[catch$sp_code %in% MAK] <- "MAK"
catch$sp_category[catch$sp_code %in% HHD] <- "HHD"

# now aggregate A categories
catch$condition_use <- catch$condition_land
catch$condition_use[catch$condition_use %in% c('A0','A1','A2','A3')] <- 'A'

# Now get rid of shark data we don't need!
catchdat <- catch[catch$sp_category!="SHK",]

# Only use sets still remaining in the set data
catchdat <- filter(catchdat, l_set_id %in% sets$l_set_id)

# Let's sort out hook position in the basket - if greater than hpb then set to NA
catchdat$hook_no[catchdat$hook_no==99] <- 0
catchdat$hook_no[catchdat$hook_no > catchdat$hk_bt_flt] <- NA # check meaning hook_no = 0

# standardise against middle of the basket
catchdat$hook_pos <- with(catchdat, ifelse(hook_no <= (hk_bt_flt/2),
                            hook_no, hk_bt_flt-hook_no+1))

##################################################
# JUST SELECT THE VARIABLES THAT WE NEED TO GIVE TO Carl
# [1] "obstrip_id"      "program_code"    "flag"            "fishery"         "vessel_id"       "l_set_id"        "set_start_date"  "set_start_time"  "set_end_time"    "haul_start_date" "haul_start_time" "lat1d"
#[13] "lon1d"           "eez_code"        "tar_sp_code"     "target_tun_yn"   "target_swo_yn"   "target_shk_yn"   "hk_bt_flt"       "hook_set"        "hook_est"        "lightsticks"     "bask_set"        "bask_observed"
#[25] "nbshark_lines"   "bait1_sp_code"   "bait2_sp_code"   "bait3_sp_code"   "bait4_sp_code"   "bait5_sp_code"   "wire_trace"      "hook_type"       "sharktarget"     "sharkbait"       "moonfrac"        "sst"
#[37] "soak"


setvar <- c("fishery","l_set_id","obstrip_id","vessel_id","flag",
            "lat1d","lon1d","eez_code","set_start_date",
            "yy","mm","set_start_time","soak","hk_bt_flt","hook_set",
            "bait1_sp_code","wire_trace","hook_type",
            "sharkbait","nbshark_lines")

set_dmp <- setdat[,setvar]



#> names(catchdat)
# [1] "obstrip_id"        "l_set_id"          "catch_time"        "sp_code"           "sp_category"       "hk_bt_flt"         "hook_no"           "condition_land"    "condition_release" "fate_code"         "condition_use"
#[12] "hook_pos"
#>
catvar <- c("l_set_id","obstrip_id","sp_category",
            "hk_bt_flt","hook_no","hook_pos",
            "condition_land","condition_use")
#catch_dmp <- catchdat[,catvar]



######################## set_dmp.Rdata
set.variables <- c(
"fishery",          #Three fisheries defined to be analayzed separately
"l_set_id",         # Unique identify to the longline set
"obstrip_id",       # Unique identifier to the fishing trip
"vessel_id",        # unique identifier to the vessel
"flag",             # flag of the vessel
"lat1d",            # latitude start of set at 1 degree res in decimal degrees
"lon1d",            # longitude start of set at 1 degree res in decimal degrees
"eez_code",         # approximate EEZ for start of set
"set_start_date",   # long character with set start data yyyymmdd
"yy",               # year of set
"mm",               # month of set
"set_start_time",   # decimal hour start of set
"soak",             # approximate soak time in decimal hours - time between start of set and start of haul
"tar_sp_code",      # target species reported for set - unsure if actually useful
"hk_bt_flt",        # hooks between floats - the greater the number the deeper the hooks can go
"hook_set",         # number of hooks used on the set
"bait1_sp_code",    # main bait used - not sure if useful - to many categories and a bit of a mess
"wire_trace",       # wire trace used n/y 0/1    - key variable
"hook_type",        # hook type circle or J shaped - key variable - would love an interaction with wire_trace, but unlikely contrast
"sharkbait",         # was any of the bait used at all considered bait used to target sharks (rather than tuna)
"nbshark_lines")    # did they have lines off the floats specifically trying to catch sharks - hook_pos=0 in the catch_dmp file



######################## catch_dmp.Rdata
catch.variables <- c(
"l_set_id",         # Unique identify to the longline set
"obstrip_id",       # Unique identifier to the fishing trip
"sp_category",      # These are the species of species groups to analyze separately
"hk_bt_flt"         # hooks between floats - the greater the number the deeper the hooks can go
,"hook_no",         # the number of the hook between the floats
"hook_pos",          # the position of the hook compared to its nearest float
"condition_land",   # the detailed info on condition at the side of the boat - don't use
"condition_use")     # sumamrised down to alive, dead, or unknown




    obs.sethaul <- read.csv("tubs-obsv-l_sethaul-mar19-2014.csv", header=FALSE)
    obs.sethaul <- obs.sethaul[,1:12]
    names(obs.sethaul) <- obs.sethaul[1,]
    obs.sethaul <- obs.sethaul[-1,]
