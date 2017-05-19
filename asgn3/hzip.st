#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/smalltalk/bin/gst -f

nl := Character nl.

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

Object subclass: OutBits [
   |bitIndex currentByte myStream|
   OutBits class >> new [
      self shouldNotImplement.
   ]
   OutBits class >> new: fileStream [
      |result|
      result := super new.
      result init: fileStream.
      ^result
   ]
   clearByte [
      bitIndex := 8.
      currentByte := 0.
   ]
   init: fileStream [
      myStream := fileStream.
      self clearByte.
   ]
   flushByte [
      bitIndex = 8 ifFalse: [
         myStream nextPutByte: currentByte.
         self clearByte.
      ]
   ]
   writeBit: bit [
      currentByte := currentByte bitAt: bitIndex put: bit.
      bitIndex := bitIndex - 1.
      bitIndex = 0 ifTrue: [self flushByte].
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

Object subclass: Leaf [
   |char count|
   char [ ^ char ]
   count [ ^ count ]

   Leaf class >> new [
      self shouldNotImplement
   ]

   Leaf class >> new: aChar count: aCount [
      |result|
      result := super new.
      result setChar: aChar andCount: aCount.
      ^result
   ]

   setChar: aChar andCount: aCount [
      char := aChar.
      count := aCount.
   ]

   <= other [
      ^ (count < other count)
      | ((count = other count) & (char <= other char))
   ]

   printBase: aStream [
      ^ aStream << self class << '(' << char << ',' << count
   ]

   printOn: aStream [
      (self printBase: aStream) << ')'.
   ]

   inorder: visitor prefix: string [
      visitor value: char value: string.
   ]

   myorder: visitor prefix: string bitstream: bs [
      visitor value: char value: string value: bs.
   ]

]

Leaf subclass: Tree [
   |left right|

   Tree class >> new: aChar count: aCount [
      self shouldNotImplement
   ]

   Tree class >> new: aChar count: aCount left: aLeft right: aRight [
      |result|
      result := super new: aChar count: aCount.
      result setLeft: aLeft andRight: aRight.
      ^ result
   ]

   setLeft: aLeft andRight: aRight [
      left := aLeft.
      right := aRight.
   ]

   printOn: aStream [
      (self printBase: aStream) << ',' << left << ',' << right << ')'.
   ]

   inorder: visitor prefix: string [
      left inorder: visitor prefix: string, '0'.
      right inorder: visitor prefix: string, '1'.
   ]

   myorder: visitor prefix: string bitstream: bs [
      left myorder: visitor prefix: string, '0' bitstream: bs.
      left myorder: visitor prefix: string, '1' bitstream: bs.
   ]

]

|is_debug is_compress is_uncompress is_display file_name infile root outfile bitstream| 

is_debug := 0.
is_compress := 0.
is_uncompress := 0.
is_display := 0.
file_name = ''.

freqtable := ZeroArray new: 257.
priority := SortedCollection new.
freqtable at: 256 put: 1.

(1 to: Smalltalk arguments size)
  do: 
  [:index |
    arg := (Smalltalk arguments at: index).
    arg = '-d' ifTrue: [is_debug := 1].
    arg = '-c' ifTrue: [is_compress := 1].
    arg = '-u' ifTrue: [is_uncompress := 1].
    arg = '-t' ifTrue: [is_display := 1].
  ].

file_name := Smalltalk arguments at: Smalltalk arguments size.
infile := FileStream open: file_name mode: FileStream read.
infile buildTable: freqtable.

freqtable keysAndValuesDo: 
[:key :value |
   (value notNil and: [value > 0]) ifTrue: 
   [
    |node|
      stdout << 'freq[' << key asCharacter visible << ']=' << value
             << nl.
    node := Leaf new: key asCharacter count: value.
    priority add: node.
   ]
].

[priority size > 1] whileTrue: 
[
  |left right newtree|
  left := priority removeFirst.
  (priority size > 0) ifTrue:
  [
    right := priority removeFirst.
    (left notNil and: [right notNil]) ifTrue: 
    [
      newtree := Tree new: left char count: left count + right count left: left right: right.
      priority add: newtree.
    ]

  ]
].

priority inspect.
root := priority removeFirst.

root inorder: [:char :string |
    stdout << '[' << char << ']=' << string << nl.
] prefix: ''.

"for now spit shit to fixed output file"
outfile := FileStream open: 'out' mode: FileStream write.
"for now make sure file is empty before we start"
outfile position: 0.
outfile truncate.

bitstream := OutBits new: outfile.

root myorder: [:char :string :bs |
    stdout << '[' << char << ']=' << string << nl.
    bs writeBit: 0.
    1 to: 8 do: [:index |
        bs writeBit: ((char asInteger) bitAt: index).
    ].
] prefix: '' bitstream: bitstream.

