/* Creates a MIDIClient object. */
#define FUNC_MIDIClientCreate 1

/* Disposes a MIDIClient object. */
#define FUNC_MIDIClientDispose 2

/* Creates an input port through which the client may receive incoming MIDI messages from any MIDI source. */
#define FUNC_MIDIInputPortCreate 3

/* Creates an output port through which the client may send outgoing MIDI messages to any MIDI destination. */
#define FUNC_MIDIOutputPortCreate 4

/* Disposes a MIDIPort object. */
#define FUNC_MIDIPortDispose 5

/* Establishes a connection from a source to a client's input port. */
#define FUNC_MIDIPortConnectSource 6

/* Closes a previously-established source-to-input port connection. */
#define FUNC_MIDIPortDisconnectSource 7

/* Returns the number of devices in the system. */
#define FUNC_MIDIGetNumberOfDevices 8

/* Returns one of the devices in the system. */
#define FUNC_MIDIGetDevice 9

/* Returns the number of entities in a given device. */
#define FUNC_MIDIDeviceGetNumberOfEntities 10

/* Returns one of a given device's entities. */
#define FUNC_MIDIDeviceGetEntity 11

/* Returns the number of sources in a given entity. */
#define FUNC_MIDIEntityGetNumberOfSources 12

/* Returns one of a given entity's sources. */
#define FUNC_MIDIEntityGetSource 13

/* Returns the number of destinations in a given entity. */
#define FUNC_MIDIEntityGetNumberOfDestinations 14

/* Returns one of a given entity's destinations. */
#define FUNC_MIDIEntityGetDestination 15

/* Returns an entity's device. */
#define FUNC_MIDIEntityGetDevice 16

/* Returns the number of sources in the system. */
#define FUNC_MIDIGetNumberOfSources 17

/* Returns one of the sources in the system. */
#define FUNC_MIDIGetSource 18

/* Returns the number of destinations in the system. */
#define FUNC_MIDIGetNumberOfDestinations 19

/* Returns one of the destinations in the system. */
#define FUNC_MIDIGetDestination 20

/* Returns an endpoint's entity. */
#define FUNC_MIDIEndpointGetEntity 21

/* Creates a virtual destination in a client. */
#define FUNC_MIDIDestinationCreate 22

/* Creates a virtual source in a client. */
#define FUNC_MIDISourceCreate 23

/* Disposes a virtual source or destination your client created. */
#define FUNC_MIDIEndpointDispose 24

/* Returns the number of external MIDI devices in the system. */
#define FUNC_MIDIGetNumberOfExternalDevices 25

/* Returns one of the external devices in the system. */
#define FUNC_MIDIGetExternalDevice 26

/* Gets an object's integer-type property. */
#define FUNC_MIDIObjectGetIntegerProperty 27

/* Sets an object's integer-type property. */
#define FUNC_MIDIObjectSetIntegerProperty 28

/* Gets an object's string-type property. */
#define FUNC_MIDIObjectGetStringProperty 29

/* Sets an object's string-type property. */
#define FUNC_MIDIObjectSetStringProperty 30

/* Gets an object's data-type property. */
#define FUNC_MIDIObjectGetDataProperty 31

/* Sets an object's data-type property. */
#define FUNC_MIDIObjectSetDataProperty 32

/* Gets an object's dictionary-type property. */
#define FUNC_MIDIObjectGetDictionaryProperty 33

/* Sets an object's dictionary-type property. */
#define FUNC_MIDIObjectSetDictionaryProperty 34

/* Gets all of an object's properties. */
#define FUNC_MIDIObjectGetProperties 35

/* Removes an object's property. */
#define FUNC_MIDIObjectRemoveProperty 36

/* Locates a device, external device, entity, or endpoint by its uniqueID.  */
#define FUNC_MIDIObjectFindByUniqueID 37

/* Sends MIDI to a destination. */
#define FUNC_MIDISend 38

/* Sends a single system-exclusive event, asynchronously. */
#define FUNC_MIDISendSysex 39

/* Distributes incoming MIDI from a source to the client input ports which are connected to that source. */
#define FUNC_MIDIReceived 40

/* Unschedules previously-sent packets. */
#define FUNC_MIDIFlushOutput 41

/* Stops and restarts MIDI I/O. */
#define FUNC_MIDIRestart 42

/* Advances a MIDIPacket pointer to the MIDIPacket which immediately follows it in memory if it is part of a MIDIPacketList. */
#define FUNC_MIDIPacketNext 43

/* Prepares a MIDIPacketList to be built up dynamically. */
#define FUNC_MIDIPacketListInit 44

/* Adds a MIDI event to a MIDIPacketList. */
#define FUNC_MIDIPacketListAdd 45

#define ERR_kMIDIInvalidClient -10830
#define ERR_kMIDIInvalidPort -10831
#define ERR_kMIDIWrongEndpointType -10832
#define ERR_kMIDINoConnection -10833
#define ERR_kMIDIUnknownEndpoint -10834
#define ERR_kMIDIUnknownProperty -10835
#define ERR_kMIDIWrongPropertyType -10836
#define ERR_kMIDINoCurrentSetup -10837
#define ERR_kMIDIMessageSendErr -10838
#define ERR_kMIDIServerStartErr -10839
#define ERR_kMIDISetupFormatErr -10840
#define ERR_kMIDIWrongThread -10841
#define ERR_kMIDIObjectNotFound -10842
#define ERR_kMIDIIDNotUnique -1084
