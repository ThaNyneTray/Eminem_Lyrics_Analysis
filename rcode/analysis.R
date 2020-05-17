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


# 1. here we use unnest tokens to get a column of words (tidy text) on its own, which 
#    streamlines the analysis process
# 2. we also remove stop words, using anti join

data("stop_words")

tidy_lyrics<-lyrics %>%
  unnest_tokens(word, lyric) %>%
  anti_join(stop_words) 

words_total<-tidy_lyrics %>%
  count(word) %>%
  arrange(-n)

words_total
# 3. In true Eminem fashion, we find some curses amongst his top 10 most used
# 4. Also notice that no 2 and 4 are practically the same. THis would be the 
#    perfect time to add some lemmatization

# words_ld<-tidy_lyrics %>%
#   mutate(lemma=textstem::lemmatize_words(word)) %>%
#   select(word, lemma) %>%
#   filter(word != lemma) %>%
#   distinct(word, lemma)

# 5. let;s see what difference it makes in total count
words_total_lemma<-tidy_lyrics %>%
  mutate(lemma=textstem::lemmatize_words(word)) %>%
  # group_by(lemma) %>%
  count(lemma) %>%
  arrange(-n)

# 6. THe lemmatization caught  some extra variations of some words,
#    However, it missed "fucking" because of the missing 'g' in its gerund
# 7. We can modify this manually. At the same time, yeah was corrected to yeah,
#    I don't like that. So we correct it.
# 8. First, store the unlemmatized lyrics, in case we need them later
lyrics_unlemma<-tidy_lyrics

tidy_lyrics<-tidy_lyrics %>%
  mutate(word=textstem::lemmatize_words(word),
         word=ifelse(word=="yes", "yeah", word),
         word=ifelse(word=="fuckin", "fuck", word))

# 9. Finally, we get the frequency, and split by album
# 10.TOtal frequency
tidy_lyrics %>%
  count(word) %>%
  arrange(-n)

# 11. Let's see the frequency difference across albums
# 12. It might be more reasonable to look at the proportions as
#     opposed to raw counts. The album lengths are bound to be different
# 13. Basically, which words appear in highest proportion in each album?
freq<-tidy_lyrics %>%
  count(album, word) %>%
  # arrange(-n) %>%
  group_by(album) %>%
  mutate(total=sum(n)) %>%
  # ungroup() %>%
  mutate(freq=n/total)

freq
# 14. To make it easier to see, let's plot it
#     TODO: come back to this. How to rearrange plot
freq %>%
  group_by(album) %>%
  # top_n(10, freq) %>%
  arrange(-freq) %>%
  # mutate(word=factor(word, freq)) %>%
  ggplot(aes(word, freq, fill=album)) +
  geom_col(show.legend = FALSE) +
  # facet_wrap(~album, ncol=3, scales="free") +
  coord_flip() + theme_bw()

# 15. let's see about word correlations,
#     We wanna spread the values of the album row to take individual columns.
freq<-freq %>%
  select(album, word, freq) %>%
  spread(album, freq, fill=0)

# 16. Here since we are using a bivariate plot, let's choose some albums to compare
#     WIth how terriby Revival was received, one of the mottos was 
#     "Bring the Real Slim Shady back". So let's see how it compares to 
#     Slim Shady LP
sslp_revival<-freq %>%
  select(word, `The Slim Shady LP`, Revival) %>%
  arrange(-Revival, -`The Slim Shady LP`) %>%
  filter(!Revival==0 & !`The Slim Shady LP`==0)

ggplot(sslp_revival, aes(`The Slim Shady LP`, Revival)) +
  geom_jitter(alpha=0.1, size=2.5, width=0.25, height=0.25)+
  geom_text(aes(label=word), check_overlap=TRUE, vjust=1.5) +
  scale_x_log10(labels=scales::percent_format())+
  scale_y_log10(labels=scales::percent_format())+
  geom_abline(color="red") +
  theme_bw()

# 17. Since this is something we might have to do repeatedly for different pairs of 
#     albums, let's wrap it in a function
albums_corplot<-function(album1, album2, freq){
  album1<-enquo(album1)
  album2<-enquo(album2)
  
  freq<-freq %>%
    select(word, !!album1, !!album2) %>%
    filter(!(!!album1==0) & !(!!album2==0))
  print(album1)
  print(album2)
  ggplot(freq, aes(!!album1, !!album2)) +
    geom_jitter(alpha=0.1, size=2.5, width=0.25, height=0.25)+
    geom_text(aes(label=word), check_overlap=TRUE, vjust=1.5) +
    scale_x_log10(labels=scales::percent_format())+
    scale_y_log10(labels=scales::percent_format())+
    geom_abline(color="red") +
    theme_bw()
}

albums_corplot(`The Slim Shady LP`, Revival, freq)
albums_corplot(`The Marshall Mathers LP`, `The Marshall Mathers LP2`, freq)
albums_corplot(Kamikaze, Revival, freq)

##################################################################################
############## COMPARING WORD USAGE ##############################################
##################################################################################

# 18. Here we wanna check which words are more likely to show up in which 
#     albums. We use log odds ratio
word_ratios<-tidy_lyrics %>%
  count(word, album) %>%
  group_by(word) %>%
  filter(sum(n)>10) %>%
  ungroup() %>%
  spread(album, n, fill=0) %>%
  mutate_if(is.numeric, list(~(. + 1)/(sum(.)+1))) %>%
  mutate(logratio=log(`The Slim Shady LP`/Revival)) %>%
  arrange(desc(logratio)) %>%
  select(word, `The Slim Shady LP`, Revival, logratio)
  
word_ratios %>%
  arrange(abs(logratio))

# 19. Let's plot the top 15 in these
word_ratios %>%
  group_by(logratio<0) %>%
  top_n(10, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill=logratio<0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + theme_bw() +
  ylab("log odds ratio (The Slim Shady LP/Revival)") +
  scale_fill_discrete(name="", labels=c("The Slim Shady LP", "Revival"))
  
# 20. Wrap this in a function
get_log_odds<-function(dat, album1, album2){
  album1<-enquo(album1)
  album2<-enquo(album2)
  
  word_ratios<-dat %>%
    count(word, album) %>%
    group_by(word) %>%
    filter(sum(n)>10) %>%
    ungroup() %>%
    spread(album, n, fill=0) %>%
    mutate_if(is.numeric, list(~(. + 1)/(sum(.)+1))) %>%
    mutate(logratio=log(!!album1/!!album2)) %>%
    arrange(desc(logratio)) %>%
    select(word, !!album1, !!album2, logratio)
  
  word_ratios
}

word_ratios<-get_log_odds(tidy_lyrics, `The Marshall Mathers LP`, `The Marshall Mathers LP2`)

plot_log_odds<-function(word_ratios, album1, album2){
  # album1<-enquo(album1)
  # album2<-enquo(album2)
  
  word_ratios %>%
    group_by(logratio<0) %>%
    top_n(15, abs(logratio)) %>%
    ungroup() %>%
    mutate(word = reorder(word, logratio)) %>%
    ggplot(aes(word, logratio, fill=logratio<0)) +
    geom_col(show.legend = FALSE) +
    coord_flip() + theme_bw() +
    ylab(paste0("log odds ratio (", album1,"/", album2,")" )) +
    scale_fill_discrete(name="", labels=c(album1, album2))
}

plot_log_odds(word_ratios, "The Marshall Mathers LP", "The Marshall Mathers LP2")


##################################################################################
############## CHANGES IN WORD USE OVER TIME #####################################
##################################################################################

##################################################################################
############## SENTIMENT ANALYSIS ################################################
##################################################################################

##################################################################################
############## TOPIC MODELING ####################################################
##################################################################################