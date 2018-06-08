unit Events;

interface

uses Classes;

type
    TCaseCreatedEvent = procedure(caseDto: TCase_Dto) of object;
    TCaseCompletedEvent = procedure(caseDto: TCase_Dto) of object;
    TCaseDeletedEvent = procedure(caseDto: TCase_Dto) of object;
implementation

end.
