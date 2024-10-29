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

program cloudmailer;

uses
  Forms,
  cloudmailerf in 'cloudmailerf.pas' {FormCloudmailer};

begin
  Application.Initialize;

  Application.CreateForm(TFormCloudmailer, FormCloudmailer);
  Application.Run;
end.


         
