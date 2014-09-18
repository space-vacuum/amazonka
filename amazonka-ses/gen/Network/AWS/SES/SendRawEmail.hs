{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SES.SendRawEmail
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sends an email message, with header and content specified by the client.
-- The SendRawEmail action is useful for sending multipart MIME emails. The
-- raw text of the message must comply with Internet email standards;
-- otherwise, the message cannot be sent. You can only send email from
-- verified email addresses and domains. If you have not requested production
-- access to Amazon SES, you must also verify every recipient email address
-- except for the recipients provided by the Amazon SES mailbox simulator. For
-- more information, go to the Amazon SES Developer Guide. The total size of
-- the message cannot exceed 10 MB. This includes any attachments that are
-- part of the message. Amazon SES has a limit on the total number of
-- recipients per message: The combined number of To:, CC: and BCC: email
-- addresses cannot exceed 50. If you need to send an email message to a
-- larger audience, you can divide your recipient list into groups of 50 or
-- fewer, and then call Amazon SES repeatedly to send the message to each
-- group. The To:, CC:, and BCC: headers in the raw message can contain a
-- group list. Note that each recipient in a group list counts towards the
-- 50-recipient limit. For every message that you send, the total number of
-- recipients (To:, CC: and BCC:) is counted against your sending quota - the
-- maximum number of emails you can send in a 24-hour period. For information
-- about your sending quota, go to the Amazon SES Developer Guide. POST /
-- HTTP/1.1 Date: Wed, 17 Aug 2011 00:21:38 GMT Host:
-- email.us-east-1.amazonaws.com Content-Type:
-- application/x-www-form-urlencoded X-Amzn-Authorization: AWS3
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE,
-- Signature=uN0lHIf14tmMBzwnkHzaWBLrBFvJAvyXCsfSYAvwLuc=,
-- Algorithm=HmacSHA256, SignedHeaders=Date;Host Content-Length: 230
-- AWSAccessKeyId=AKIAIOSFODNN7EXAMPLE &Action=SendRawEmail
-- &RawMessage.Data=U3ViamVjdDogRXhhbXBsZQpGcm9tOiBleGFtcGxlQGFtYXpvbi5jb20KVG8
-- 
-- 6IGV4YW1wbGVAYW1h%0Aem9uLmNvbQpDb250ZW50LVR5cGU6IG11bHRpcGFydC9hbHRlcm5hdGl2
-- 
-- ZTsgYm91bmRhcnk9MDAx%0ANmU2OGY5ZDkyOWNiMDk2MDRhYWE4MzA0MgoKLS0wMDE2ZTY4ZjlkO
-- 
-- TI5Y2IwOTYwNGFhYTgzMDQy%0ACkNvbnRlbnQtVHlwZTogdGV4dC9wbGFpbjsgY2hhcnNldD1JU0
-- 
-- 8tODg1OS0xCgpCb2R5LgoKLS0w%0AMDE2ZTY4ZjlkOTI5Y2IwOTYwNGFhYTgzMDQyCkNvbnRlbnQ
-- 
-- tVHlwZTogdGV4dC9odG1sOyBjaGFy%0Ac2V0PUlTTy04ODU5LTEKCkJvZHkuPGJyPgoKLS0wMDE2
-- ZTY4ZjlkOTI5Y2IwOTYwNGFhYTgzMDQy%0ALS0%3D%0A
-- &Timestamp=2011-08-17T00%3A21%3A38.000Z
-- 00000131d51d6b36-1d4f9293-0aee-4503-b573-9ae4e70e9e38-000000
-- e0abcdfa-c866-11e0-b6d0-273d09173b49.
module Network.AWS.SES.SendRawEmail
    (
    -- * Request
      SendRawEmail
    -- ** Request constructor
    , sendRawEmail
    -- ** Request lenses
    , sreSource
    , sreDestinations
    , sreRawMessage

    -- * Response
    , SendRawEmailResponse
    -- ** Response constructor
    , sendRawEmailResponse
    -- ** Response lenses
    , srerMessageId
    ) where

import Network.AWS.Request.Query
import Network.AWS.SES.Types
import Network.AWS.Prelude

-- | Represents a request instructing the service to send a raw email message.
-- This datatype can be used in application code to compose a message
-- consisting of source, destination, and raw message text. This object can
-- then be sent using the SendRawEmail action.
data SendRawEmail = SendRawEmail
    { _sreSource :: Maybe Text
    , _sreDestinations :: [Text]
    , _sreRawMessage :: RawMessage
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SendRawEmail' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Source ::@ @Maybe Text@
--
-- * @Destinations ::@ @[Text]@
--
-- * @RawMessage ::@ @RawMessage@
--
sendRawEmail :: RawMessage -- ^ 'sreRawMessage'
               -> SendRawEmail
sendRawEmail p3 = SendRawEmail
    { _sreSource = Nothing
    , _sreDestinations = mempty
    , _sreRawMessage = p3
    }

-- | The identity's email address. By default, the string must be 7-bit ASCII.
-- If the text must contain any other characters, then you must use MIME
-- encoded-word syntax (RFC 2047) instead of a literal string. MIME
-- encoded-word syntax uses the following form:
-- =?charset?encoding?encoded-text?=. For more information, see RFC 2047. If
-- you specify the Source parameter and have feedback forwarding enabled, then
-- bounces and complaints will be sent to this email address. This takes
-- precedence over any Return-Path header that you might include in the raw
-- text of the message.
sreSource :: Lens' SendRawEmail (Maybe Text)
sreSource = lens _sreSource (\s a -> s { _sreSource = a })

-- | A list of destinations for the message, consisting of To:, CC:, and BCC:
-- addresses.
sreDestinations :: Lens' SendRawEmail [Text]
sreDestinations = lens _sreDestinations (\s a -> s { _sreDestinations = a })

-- | The raw text of the message. The client is responsible for ensuring the
-- following: Message must contain a header and a body, separated by a blank
-- line. All required header fields must be present. Each part of a multipart
-- MIME message must be formatted properly. MIME content types must be among
-- those supported by Amazon SES. For more information, go to the Amazon SES
-- Developer Guide. Content must be base64-encoded, if MIME requires it.
sreRawMessage :: Lens' SendRawEmail RawMessage
sreRawMessage = lens _sreRawMessage (\s a -> s { _sreRawMessage = a })

instance ToQuery SendRawEmail where
    toQuery = genericQuery def

-- | Represents a unique message ID returned from a successful SendRawEmail
-- request.
newtype SendRawEmailResponse = SendRawEmailResponse
    { _srerMessageId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SendRawEmailResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @MessageId ::@ @Text@
--
sendRawEmailResponse :: Text -- ^ 'srerMessageId'
                       -> SendRawEmailResponse
sendRawEmailResponse p1 = SendRawEmailResponse
    { _srerMessageId = p1
    }

-- | The unique message identifier returned from the SendRawEmail action.
srerMessageId :: Lens' SendRawEmailResponse Text
srerMessageId = lens _srerMessageId (\s a -> s { _srerMessageId = a })

instance FromXML SendRawEmailResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest SendRawEmail where
    type Sv SendRawEmail = SES
    type Rs SendRawEmail = SendRawEmailResponse

    request = post "SendRawEmail"
    response _ = xmlResponse
