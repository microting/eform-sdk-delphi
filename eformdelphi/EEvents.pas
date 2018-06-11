unit EEvents;

interface

uses Classes;

type
    TCaseCreatedEvent = procedure(caseDto: TCase_Dto) of object;
    TCaseCompletedEvent = procedure(caseDto: TCase_Dto) of object;
    TCaseDeletedEvent = procedure(caseDto: TCase_Dto) of object;
    TCaseRetrivedEvent = procedure(caseDto: TCase_Dto) of object;
    TEventExceptionEvent = procedure(error: string) of object;
    TSiteActivatedEvent = procedure(siteId: integer) of object;
    TFileDownloadedEvent = procedure(caseDto: TFile_Dto) of object;
    TNotificationNotFoundEvent = procedure(caseDto: TNote_Dto) of object;
implementation

end.
