module Exercise02.MediaLibrary where

-- TODO should be rewritten

import Data.Maybe
import Data.List

type TrackTitle = String
type AlbumTitle = String
type Artist = String
type Duration = Int
type User = String

data Track = Track {
                 trackTitle :: TrackTitle,
                 trackArtist :: Artist,
                 trackDuration :: Duration
             } deriving (Show, Eq)

data Album = Album {
                 albumTitle :: AlbumTitle,
                 albumTracks :: [Track]
             }
             deriving (Show, Eq)

data Review = Review {
                  revUser :: User,
                  revTrack :: Track,
                  revRating :: Rating
              } deriving (Show, Eq)

data MediaBib = MediaBib {
                    bibTracks :: [Track],
                    bibAlbums :: [Album],
                    bibReviews :: [Review]
                } deriving (Show, Eq)

data Rating = Good | Bad
    deriving (Show, Eq)

-- some tracks & albums
track1 = Track "Knocking on Your Back Door" "Deep Purple" 180
track2 = Track "High and Dry" "Radiohead" 420
track3 = Track "Heaven&Hell" "Black Sabbath" 666

album1 = Album "Best of Rock" [track1]
album2 = Album "Best of Rock vol. 2" [track1, track2]
 
initBib = MediaBib [] [] []
bib1 = addAlbumToBib album1 initBib
bib1'= addAlbumToBib album2 initBib
bib2 = rateTrack "Borna" track1 Good bib1
bib2' = rateTrack "Born" track1 Good bib1'
goodAlbums = findGoodAlbums "Borna" bib2
goodAlbums' = findGoodAlbums "Borna" bib2'

-- FUNCTIONS

-- add an album to the bib
addAlbumToBib :: Album -> MediaBib -> MediaBib
addAlbumToBib album bib = bib { bibAlbums = newAlbums }
    where oldAlbums = bibAlbums bib
          newAlbums = oldAlbums ++ [album]

-- add the song to the album already in the bib
addTrackToBibAlbum :: Track -> Album -> MediaBib -> (Album, MediaBib)
addTrackToBibAlbum track album bib = (newAlbum, bib { bibAlbums = newAlbums })
    where oldAlbums = bibAlbums bib
          oldAlbum = fromJust $ find (\x -> albumTitle x == albumTitle album) oldAlbums
          oldAlbumTracks = albumTracks oldAlbum
          newAlbumTracks = oldAlbumTracks ++ [track]
          newAlbum = oldAlbum { albumTracks = newAlbumTracks }
          newAlbums = (filter (/= oldAlbum) oldAlbums) ++ [newAlbum]

-- add new rating from the user
rateTrack :: User -> Track -> Rating -> MediaBib -> MediaBib
rateTrack user track rating bib = bib { bibReviews = newReviews } 
    where newReview = Review user track rating
          oldReviews = bibReviews bib
          newReviews = oldReviews ++ [newReview]


-- get all albums and their durations
albumsAndDurations bib = albumsAndDurationsIter albums
   where albums = bibAlbums bib

albumsAndDurationsIter [] = []
albumsAndDurationsIter (a:as) = (title, duration):(albumsAndDurationsIter as)
    where title = albumTitle a
          tracks = albumTracks a
          duration = sumTrackDurationsIter tracks


sumTrackDurationsIter [] = 0
sumTrackDurationsIter (t:ts) = (trackDuration t) + sumTrackDurationsIter ts

-- ˇˇˇ
-- return all albums which songs have more than 50% good reviews by the given user
findGoodAlbums :: User -> MediaBib -> [Album]
findGoodAlbums user bib = goodAlbumsIter user reviews albums [] 
    where reviews = bibReviews bib
          albums = bibAlbums bib

-- find all good albums
goodAlbumsIter :: User -> [Review] -> [Album] -> [Album] -> [Album]
goodAlbumsIter user reviews [] goodAlbums = goodAlbums
goodAlbumsIter user reviews (a:as) goodAlbums = goodAlbumsIter user reviews as newGoodAlbums
    where songs = albumTracks a
          numGoodSongs = fromIntegral $ goodSongIter user reviews songs 0
          numTotalSongs = fromIntegral $ length songs
          goodAlbum = (numGoodSongs / numTotalSongs) > 0.5
          newGoodAlbums = if goodAlbum then (goodAlbums ++ [a]) else goodAlbums

-- determine how many good songs are there
goodSongIter :: User -> [Review] -> [Track] -> Integer -> Integer
goodSongIter user reviews [] numGoodSongs = numGoodSongs
goodSongIter user reviews (s:ss) numGoodSongs = goodSongIter user reviews ss newNumGoodSongs
    where songIsGood = goodRevIter user s reviews
          newNumGoodSongs = if songIsGood then (numGoodSongs + 1) else numGoodSongs

-- determine if user likes the song
goodRevIter :: User -> Track -> [Review] -> Bool
goodRevIter user track [] = False
goodRevIter user track (r:rs) = if revUser r == user && revTrack r == track && revRating r == Good
                                then True
                                else goodRevIter user track rs
