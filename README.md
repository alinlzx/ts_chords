# ts_chords
 A Chordal Analysis of Taylor Swift's Discography 


 # About 

 This repo contains data and relevant visualizations/analyses for 147 Taylor Swift songs. The data combines numerical metrics from Spotify API and my interpretations of which chord/the number of chords Taylor Swift used. 

 The full dataset can be found under `ts_chords/build/mst`

 Chords are identified based on the following methodology: 

## How chords are identified

I studied the chords of every song Taylor has released as part of original studio albums, from Taylor Swift to Midnights.

The chordal breakdown of Taylor’s entire song collection comes from Ultimate Guitar.

For each song, I record the following characteristics based on the most primitive, Roman numeral forms of Taylor’s chordal components; this means that I disregard inversions, ninths/sevenths, suspensions, or modulations: 

- How many distinct chords are used in the entire song.
- How many of said distinct chords are not one of the four ubiquitous chords, i.e., I, V, IV, and vi.
- What is the chord progression of the chorus.

Steps 3) is by far the most complicated because even Taylor’s ostensibly simple chord progressions are littered with irregularities. To avoid making too many arbitrary judgment calls, I adhere to the following procedure as closely as possible: 

- Identify the part of the song that is the chorus. Sometimes, the users from Ultimate Guitar label the chorus. Other times, I identify the chorus by picking out segments where the lyric repeats. 
- Identify a I chord in the chorus, if possible (the vast majority of times, there are I chords in the chorus because tonic is almost always the most important and the most commonly used note in tonal music). 
- Find the shortest sequence of chords trailing the I chord that is repeated. This is the chord progression (inevitably, there are sometimes straggler chords in the chorus that do not get captured by this chord progression). 

