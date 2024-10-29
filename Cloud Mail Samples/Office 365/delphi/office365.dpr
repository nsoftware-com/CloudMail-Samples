(*
 * Cloud Mail 2024 Delphi Edition - Sample Project
 *
 * This sample project demonstrates the usage of Cloud Mail in a 
 * simple, straightforward way. It is not intended to be a complete 
 * application. Error handling and other checks are simplified for clarity.
 *
 * www.nsoftware.com/cloudmail
 *
 * This code is subject to the terms and conditions specified in the 
 * corresponding product license agreement which outlines the authorized 
 * usage and restrictions.
 *)

program office365;

uses
  Forms,
  composef in 'composef.pas'   {FormComposef},
  office365f in 'office365f.pas' {FormOffice365};

begin
  Application.Initialize;

  Application.CreateForm(TFormOffice365, FormOffice365);
  Application.CreateForm(TFormCompose, FormCompose);

  Application.Run;
end.


         
