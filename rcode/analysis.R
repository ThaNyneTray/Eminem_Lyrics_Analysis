library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(geniusr)
library(stringr)
library(Rspotify)
library(readr)
library(forcats)


##################################################################################
############## GET LYRICS ########################################################
##################################################################################

#### specify authorization key ###################################################
# keys<-spotifyOAuth(app_id="EvolutionOfHipHop",
#                    client_id="fb596ca0990a475fa1203b3dec8370a9",
#                    client_secret="a94a49dc86cf42e087dd68a30178fd5c")

# we get the album list from  Spotify - genius API lacks this feature
# id<-searchArtist("Eminem", keys)$id[1]
# albums<-getAlbums(artist_id=id, token=keys)
# albums<-unique(albums$name)
# albums<-albums[c(1,2,3,5,6,8,11,12,14,16)]


# track_list<-NULL
# for (i in 1:length(albums)){
#   tracks<-get_album_tracklist_search("Eminem", albums[i])
#   tracks$album<-albums[i]
#   track_list<-rbind(track_list, tracks)
# }
# track_list
# 
# # 
# # # save the track_list data to a csv
# write.csv(track_list, file="./data/track_list_data.csv", row.names=F)

## load the track_list data frame
# track_list<-read.csv("./data/track_list_data.csv", stringsAsFactors=F)


# get lyrics for the entire tracklist, and combine them into a data frame
# lyrics<-c()
# for (i in 1:nrow(track_list)){
#   album_lyric<-get_lyrics_url(track_list$song_lyrics_url[i]) %>%
#     mutate(album=track_list$album[i],
#            track_number=track_list$song_number[i])
#   lyrics<-rbind(lyrics, album_lyric)
# }

##################################################################################
############## DATA IMPORT & CLEANING ############################################
##################################################################################

# DATA CLEANING
# 1. first, we are analyzing Eminem lyrics, so we want Eminem's words
#    he is known to write a lot of the hooks as well, but we can't be sure
# 2. we also don't need that column anymore, and the song urls,
#    or the artist name, so we remove it
# 3. need line numbers for analysis later, so we add those, 
 
# lyrics<-lyrics %>%
#   filter(section_artist=="Eminem") %>%
#   rename(lyric=line, track_n=track_number) %>%
#   group_by(song_name) %>%
#   mutate(line=cumsum(song_name==song_name)) %>%
#   select(lyric, line, album, track_n, song_name) 

# save data to csv file for future access
# write_csv(lyrics, path="./data/lyrics_by_lines.csv")
# 
# # load lyrics data
lyrics<-read_csv(file="./data/lyrics_by_lines.csv")

lyrics

##################################################################################
############## WORD FREQUENCIES ##################################################
##################################################################################

##################################################################################
############## COMPARING WORD USAGE ##############################################
##################################################################################

##################################################################################
############## CHANGES IN WORD USE OVER TIME #####################################
##################################################################################

##################################################################################
############## SENTIMENT ANALYSIS ################################################
##################################################################################

##################################################################################
############## TOPIC MODELING ####################################################
##################################################################################