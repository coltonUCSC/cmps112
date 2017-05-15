Object subclass: Meme [
	Meme class >> new 
	[
		self shouldNotImplement.
	]

	Meme class >> arg1: str arg2: maymay 
	[
	    |result|
	    result := super new.
		^ result.
	]

	spicy 
	[
		^ 'meme'.
	]
]

freqtable := Meme arg1: 'wew' arg2: 'wow'.

stdout << freqtable spicy.