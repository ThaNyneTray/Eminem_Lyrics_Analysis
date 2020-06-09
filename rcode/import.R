##################################################################################
############## GET LYRICS ########################################################
##################################################################################

#### specify authorization key ###################################################
keys<-spotifyOAuth(app_id="EvolutionOfHipHop",
                   client_id="fb596ca0990a475fa1203b3dec8370a9",
                   client_secret="a94a49dc86cf42e087dd68a30178fd5c")

# we get the album list from  Spotify - genius API lacks this feature
id<-searchArtist("Eminem", keys)$id[1]
albums<-getAlbums(artist_id=id, token=keys)
albums<-unique(albums$name)
albums<-albums[c(1,2,3,5,6,8,11,12,14,16)]

track_list<-NULL
for (i in 1:length(albums)){
  tracks<-get_album_tracklist_search("Eminem", albums[i])
  tracks$album<-albums[i]
  track_list<-rbind(track_list, tracks)
}
track_list

# save the track_list data to a csv
write.csv(track_list, file="./data/track_list_data.csv", row.names=F)

# load the track_list data frame
track_list<-read.csv("./data/track_list_data.csv", stringsAsFactors=F)


# get lyrics for the entire tracklist, and combine them into a data frame
lyrics<-c()
for (i in 1:nrow(track_list)){
  album_lyric<-get_lyrics_url(track_list$song_lyrics_url[i]) %>%
    mutate(album=track_list$album[i],
           track_number=track_list$song_number[i])
  lyrics<-rbind(lyrics, album_lyric)
}

# add album years as well
album_year<-c("01-17-2020","08-31-2018","12-15-2017","11-5-2013","06-18-2010",
              "05-15-2009","11-12-2004","05-26-2002","05-23-2000","02-23-1999")
album_year<-mdy(album_year)

album_dates<-tibble(album=albums,album_year=album_year)
#
# # add dates to the lyrics. THese are album dates. Useful later on
lyrics<-lyrics %>%
  inner_join(album_dates, by=c("album"))
#
lyrics
#
# # save the original lyric data
write_csv(lyrics, "./data/original_lyrics.csv")