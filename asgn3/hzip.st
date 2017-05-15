#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

|is_debug is_compress is_uncompress is_display file_name infile| 

is_debug := 0.
is_compress := 0.
is_uncompress := 0.
is_display := 0.
file_name = ''.

(1 to: Smalltalk arguments size)
  do: 
  [:index |
    arg := (Smalltalk arguments at: index).
    arg = '-d' ifTrue: [is_debug := 1].
    arg = '-c' ifTrue: [is_compress := 1].
    arg = '-u' ifTrue: [is_uncompress := 1].
    arg = '-t' ifTrue: [is_display := 1].
  ].

FileStream extend [
  nl := Character nl.
   buildTable: table [
      [self atEnd not] whileTrue: [
        table incr: self next asInteger.
      ]
   ]
]

Character extend [
   isPrint [
      ^ (Character space <= self) & (self <= $~)
   ]
   visible [
      self isPrint ifTrue: [^ '$', self asString]
                   ifFalse: [^ self asInteger printStringRadix: 8]
   ]
]

Object subclass: ZeroArray [
   |theArray|

   ZeroArray class >> new [
      self shouldNotImplement.
   ]

   ZeroArray class >> new: size [
      |result|
      result := super new.
      result init: size.
      ^ result
   ]

   init: size [
      theArray := Array new: size.
   ]

   size [
      ^ theArray size.
   ]

   at: index [
      ^ theArray at: index + 1.
   ]

   at: index put: value [
      ^ theArray at: index + 1 put: value.
   ]

   incr: index [
      (self at: index) isNil
         ifTrue: [ self at: index put: 0].
      self at: index put: (self at: index) + 1.
   ]

   keysAndValuesDo: aBlock [
      (0 to: (self size) - 1) do: [:index |
         aBlock value: index value: (self at: index).
      ]
   ]
]

file_name := Smalltalk arguments at: Smalltalk arguments size.
infile := FileStream open: file_name mode: FileStream read.

freqtable := ZeroArray new: 256.
infile buildTable: freqtable.

freqtable keysAndValuesDo: 
[:key :value |
   (value notNil and: [value > 0]) ifTrue: 
   [
      stdout << 'freq[' << key asCharacter visible << ']=' << value
             << nl.
   ]
].