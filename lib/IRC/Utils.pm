package IRC::Utils;

use strict;
use warnings FATAL => 'all';

use Encode qw(decode);
use Encode::Guess;

require Exporter;
use base qw(Exporter);
our @EXPORT_OK = qw(
    u_irc l_irc parse_mode_line parse_mask matches_mask matches_mask_array
    unparse_mode_line gen_mode_change parse_user is_valid_nick_name decode_irc
    is_valid_chan_name has_color has_formatting strip_color strip_formatting
    NORMAL BOLD UNDERLINE REVERSE ITALIC FIXED WHITE BLACK BLUE GREEN RED
    BROWN PURPLE ORANGE YELLOW LIGHT_GREEN TEAL LIGHT_CYAN LIGHT_BLUE PINK
    GREY LIGHT_GREY numeric_to_name name_to_numeric
);
our %EXPORT_TAGS = ( ALL => [@EXPORT_OK] );

use constant {
    # cancel all formatting and colors
    NORMAL      => "\x0f",

    # formatting
    BOLD        => "\x02",
    UNDERLINE   => "\x1f",
    REVERSE     => "\x16",
    ITALIC      => "\x1d",
    FIXED       => "\x11",

    # mIRC colors
    WHITE       => "\x0300",
    BLACK       => "\x0301",
    BLUE        => "\x0302",
    GREEN       => "\x0303",
    RED         => "\x0304",
    BROWN       => "\x0305",
    PURPLE      => "\x0306",
    ORANGE      => "\x0307",
    YELLOW      => "\x0308",
    LIGHT_GREEN => "\x0309",
    TEAL        => "\x0310",
    LIGHT_CYAN  => "\x0311",
    LIGHT_BLUE  => "\x0312",
    PINK        => "\x0313",
    GREY        => "\x0314",
    LIGHT_GREY  => "\x0315",
};

# list originally snatched from AnyEvent::IRC::Util
our %NUMERIC2NAME = (
   '001' => 'RPL_WELCOME',
   '002' => 'RPL_YOURHOST',
   '003' => 'RPL_CREATED',
   '004' => 'RPL_MYINFO',
   '005' => 'RPL_BOUNCE',
   '200' => 'RPL_TRACELINK',
   '201' => 'RPL_TRACECONNECTING',
   '202' => 'RPL_TRACEHANDSHAKE',
   '203' => 'RPL_TRACEUNKNOWN',
   '204' => 'RPL_TRACEOPERATOR',
   '205' => 'RPL_TRACEUSER',
   '206' => 'RPL_TRACESERVER',
   '207' => 'RPL_TRACESERVICE',
   '208' => 'RPL_TRACENEWTYPE',
   '209' => 'RPL_TRACECLASS',
   '210' => 'RPL_TRACERECONNECT',
   '211' => 'RPL_STATSLINKINFO',
   '212' => 'RPL_STATSCOMMANDS',
   '219' => 'RPL_ENDOFSTATS',
   '221' => 'RPL_UMODEIS',
   '233' => 'RPL_SERVICE',
   '234' => 'RPL_SERVLIST',
   '235' => 'RPL_SERVLISTEND',
   '242' => 'RPL_STATSUPTIME',
   '243' => 'RPL_STATSOLINE',
   '250' => 'RPL_STATSDLINE',
   '251' => 'RPL_LUSERCLIENT',
   '252' => 'RPL_LUSEROP',
   '253' => 'RPL_LUSERUNKNOWN',
   '254' => 'RPL_LUSERCHANNELS',
   '255' => 'RPL_LUSERME',
   '256' => 'RPL_ADMINME',
   '257' => 'RPL_ADMINLOC1',
   '258' => 'RPL_ADMINLOC2',
   '259' => 'RPL_ADMINEMAIL',
   '261' => 'RPL_TRACELOG',
   '262' => 'RPL_TRACEEND',
   '263' => 'RPL_TRYAGAIN',
   '301' => 'RPL_AWAY',
   '302' => 'RPL_USERHOST',
   '303' => 'RPL_ISON',
   '305' => 'RPL_UNAWAY',
   '306' => 'RPL_NOWAWAY',
   '311' => 'RPL_WHOISUSER',
   '312' => 'RPL_WHOISSERVER',
   '313' => 'RPL_WHOISOPERATOR',
   '314' => 'RPL_WHOWASUSER',
   '315' => 'RPL_ENDOFWHO',
   '317' => 'RPL_WHOISIDLE',
   '318' => 'RPL_ENDOFWHOIS',
   '319' => 'RPL_WHOISCHANNELS',
   '321' => 'RPL_LISTSTART',
   '322' => 'RPL_LIST',
   '323' => 'RPL_LISTEND',
   '324' => 'RPL_CHANNELMODEIS',
   '325' => 'RPL_UNIQOPIS',
   '331' => 'RPL_NOTOPIC',
   '332' => 'RPL_TOPIC',
   '341' => 'RPL_INVITING',
   '342' => 'RPL_SUMMONING',
   '346' => 'RPL_INVITELIST',
   '347' => 'RPL_ENDOFINVITELIST',
   '348' => 'RPL_EXCEPTLIST',
   '349' => 'RPL_ENDOFEXCEPTLIST',
   '351' => 'RPL_VERSION',
   '352' => 'RPL_WHOREPLY',
   '353' => 'RPL_NAMREPLY',
   '364' => 'RPL_LINKS',
   '365' => 'RPL_ENDOFLINKS',
   '366' => 'RPL_ENDOFNAMES',
   '367' => 'RPL_BANLIST',
   '368' => 'RPL_ENDOFBANLIST',
   '369' => 'RPL_ENDOFWHOWAS',
   '371' => 'RPL_INFO',
   '372' => 'RPL_MOTD',
   '374' => 'RPL_ENDOFINFO',
   '375' => 'RPL_MOTDSTART',
   '376' => 'RPL_ENDOFMOTD',
   '381' => 'RPL_YOUREOPER',
   '382' => 'RPL_REHASHING',
   '383' => 'RPL_YOURESERVICE',
   '384' => 'RPL_MYPORTIS',
   '391' => 'RPL_TIME',
   '392' => 'RPL_USERSSTART',
   '393' => 'RPL_USERS',
   '394' => 'RPL_ENDOFUSERS',
   '395' => 'RPL_NOUSERS',
   '401' => 'ERR_NOSUCHNICK',
   '402' => 'ERR_NOSUCHSERVER',
   '403' => 'ERR_NOSUCHCHANNEL',
   '404' => 'ERR_CANNOTSENDTOCHAN',
   '405' => 'ERR_TOOMANYCHANNELS',
   '406' => 'ERR_WASNOSUCHNICK',
   '407' => 'ERR_TOOMANYTARGETS',
   '408' => 'ERR_NOSUCHSERVICE',
   '409' => 'ERR_NOORIGIN',
   '411' => 'ERR_NORECIPIENT',
   '412' => 'ERR_NOTEXTTOSEND',
   '413' => 'ERR_NOTOPLEVEL',
   '414' => 'ERR_WILDTOPLEVEL',
   '415' => 'ERR_BADMASK',
   '421' => 'ERR_UNKNOWNCOMMAND',
   '422' => 'ERR_NOMOTD',
   '423' => 'ERR_NOADMININFO',
   '424' => 'ERR_FILEERROR',
   '431' => 'ERR_NONICKNAMEGIVEN',
   '432' => 'ERR_ERRONEUSNICKNAME',
   '433' => 'ERR_NICKNAMEINUSE',
   '436' => 'ERR_NICKCOLLISION',
   '437' => 'ERR_UNAVAILRESOURCE',
   '441' => 'ERR_USERNOTINCHANNEL',
   '442' => 'ERR_NOTONCHANNEL',
   '443' => 'ERR_USERONCHANNEL',
   '444' => 'ERR_NOLOGIN',
   '445' => 'ERR_SUMMONDISABLED',
   '446' => 'ERR_USERSDISABLED',
   '451' => 'ERR_NOTREGISTERED',
   '461' => 'ERR_NEEDMOREPARAMS',
   '462' => 'ERR_ALREADYREGISTRED',
   '463' => 'ERR_NOPERMFORHOST',
   '464' => 'ERR_PASSWDMISMATCH',
   '465' => 'ERR_YOUREBANNEDCREEP',
   '466' => 'ERR_YOUWILLBEBANNED',
   '467' => 'ERR_KEYSET',
   '471' => 'ERR_CHANNELISFULL',
   '472' => 'ERR_UNKNOWNMODE',
   '473' => 'ERR_INVITEONLYCHAN',
   '474' => 'ERR_BANNEDFROMCHAN',
   '475' => 'ERR_BADCHANNELKEY',
   '476' => 'ERR_BADCHANMASK',
   '477' => 'ERR_NOCHANMODES',
   '478' => 'ERR_BANLISTFULL',
   '481' => 'ERR_NOPRIVILEGES',
   '482' => 'ERR_CHANOPRIVSNEEDED',
   '483' => 'ERR_CANTKILLSERVER',
   '484' => 'ERR_RESTRICTED',
   '485' => 'ERR_UNIQOPPRIVSNEEDED',
   '491' => 'ERR_NOOPERHOST',
   '492' => 'ERR_NOSERVICEHOST',
   '501' => 'ERR_UMODEUNKNOWNFLAG',
   '502' => 'ERR_USERSDONTMATCH',
);

our %NAME2NUMERIC;
while (my ($key, $val) = each %NUMERIC2NAME) {
    $NAME2NUMERIC{$val} = $key;
}

sub numeric_to_name {
   my ($code) = @_;
   return $NUMERIC2NAME{$code};
}

sub name_to_numeric {
   my ($name) = @_;
   return $NAME2NUMERIC{$name};
}

sub u_irc {
    my ($value, $type) = @_;
    return if !defined $value;
    $type = 'rfc1459' if !defined $type;
    $type = lc $type;

    if ($type eq 'ascii') {
        $value =~ tr/a-z/A-Z/;
    }
    elsif ($type eq 'strict-rfc1459') {
        $value =~ tr/a-z{}|/A-Z[]\\/;
    }
    else {
        $value =~ tr/a-z{}|^/A-Z[]\\~/;
    }

    return $value;
}

sub l_irc {
    my ($value, $type) = @_;
    return if !defined $value;
    $type = 'rfc1459' if !defined $type;
    $type = lc $type;

    if ($type eq 'ascii') {
        $value =~ tr/A-Z/a-z/;
    }
    elsif ($type eq 'strict-rfc1459') {
        $value =~ tr/A-Z[]\\/a-z{}|/;
    }
    else {
        $value =~ tr/A-Z[]\\~/a-z{}|^/;
    }

    return $value;
}

sub parse_mode_line {
    my @args = @_;

    my $chanmodes = [qw(beI k l imnpstaqr)];
    my $statmodes = 'ohv';
    my $hashref = { };
    my $count = 0;

    while (my $arg = shift @args) {
        if ( ref $arg eq 'ARRAY' ) {
           $chanmodes = $arg;
           next;
        }
        elsif (ref $arg eq 'HASH') {
           $statmodes = join '', keys %{ $arg };
           next;
        }
        elsif ($arg =~ /^[-+]/ or $count == 0) {
            my $action = '+';
            for my $char (split //, $arg) {
                if ($char eq '+' or $char eq '-') {
                   $action = $char;
                }
                else {
                   push @{ $hashref->{modes} }, $action . $char;
                }

                if (length $chanmodes->[0] && length $chanmodes->[1] && length $statmodes
                    && $char =~ /[$statmodes$chanmodes->[0]$chanmodes->[1]]/) {
                    push @{ $hashref->{args} }, shift @args;
                }

                if (length $chanmodes->[2] && $action eq '+' && $char =~ /[$chanmodes->[2]]/) {
                    push @{ $hashref->{args} }, shift @args;
                }
            }
        }
        else {
            push @{ $hashref->{args} }, $arg;
        }
        $count++;
    }

    return $hashref;
}

sub parse_mask {
    my ($arg) = @_;
    return if !defined $arg;

    $arg =~ s/\*{2,}/*/g;
    my @mask;
    my $remainder;
    if ($arg !~ /!/ and $arg =~ /@/) {
        $remainder = $arg;
    }
    else {
        ($mask[0], $remainder) = split /!/, $arg, 2;
    }

    $remainder =~ s/!//g if defined $remainder;
    @mask[1..2] = split(/@/, $remainder, 2) if defined $remainder;
    $mask[2] =~ s/@//g if defined $mask[2];

    for my $i (1..2) {
        $mask[$i] = '*' if !$mask[$i];
    }
    return $mask[0] . '!' . $mask[1] . '@' . $mask[2];
}

sub unparse_mode_line {
    my ($line) = @_;
    return if !defined $line || !length $line;

    my $action; my $return;
    for my $mode ( split(//,$line) ) {
       if ($mode =~ /^(\+|-)$/ && (!$action || $mode ne $action)) {
         $return .= $mode;
         $action = $mode;
         next;
       }
       $return .= $mode if ($mode ne '+' and $mode ne '-');
    }
    $return =~ s/[+-]$//;
    return $return;
}

sub gen_mode_change {
    my ($before, $after) = @_;
    $before = '' if !defined $before;
    $after = '' if !defined $after;

    my @before = split //, $before;
    my @after  = split //, $after;
    my $string = '';
    my @hunks = _diff(\@before, \@after);
    $string .= $_->[0] . $_->[1] for @hunks;

    return unparse_mode_line($string);
}

sub is_valid_nick_name {
    my ($nickname) = @_;
    return if !defined $nickname || !length $nickname;
    return 1 if $nickname =~ /^[A-Za-z_0-9`\-^\|\\\{}\[\]]+$/;
    return;
}

sub is_valid_chan_name {
    my $channel = shift;
    my $chantypes = shift || ['#', '&'];
    return if !@$chantypes;
    my $chanprefix = join '', @$chantypes;
    return if !defined $channel || !length $channel;

    return 1 if $channel =~ /^[$chanprefix][^ \a\0\012\015,]+$/;
    return;
}

sub matches_mask_array {
    my ($masks, $matches, $mapping) = @_;

    return if !defined $masks || !defined $matches;
    return if ref $masks ne 'ARRAY';
    return if ref $matches ne 'ARRAY';
    my $ref = { };

    for my $mask (@$masks) {
        for my $match (@$matches) {
            if (matches_mask($mask, $match, $mapping)) {
                push @{ $ref->{ $mask } }, $match;
            }
        }
    }

    return $ref;
}

sub matches_mask {
    my ($mask, $match, $mapping) = @_;
    return if !defined $mask || !length $mask;
    return if !defined $match || !length $match;

    $mask = parse_mask($mask);
    $mask =~ s/\*+/*/g;

    my $umask = quotemeta u_irc($mask, $mapping);
    $umask =~ s/\\\*/[\x01-\xFF]{0,}/g;
    $umask =~ s/\\\?/[\x01-\xFF]{1,1}/g;
    $match = u_irc($match, $mapping);

    return 1 if $match =~ /^$umask$/;
    return;
}

sub parse_user {
    my ($user) = @_;
    return if !defined $user;

    my ($n, $u, $h) = split /[!@]/, $user;
    return ($n, $u, $h) if wantarray();
    return $n;
}

sub has_color {
    my ($string) = @_;
    return if !defined $string;
    return 1 if $string =~ /[\x03\x04\x1B]/;
    return;
}

sub has_formatting {
    my ($string) = @_;
    return if !defined $string;
    return 1 if $string =~/[\x02\x1f\x16\x1d\x11]/;
    return;
}

sub strip_color {
    my ($string) = @_;
    return if !defined $string;

    # mIRC colors
    $string =~ s/\x03(?:,\d{1,2}|\d{1,2}(?:,\d{1,2})?)?//g;

    # RGB colors supported by some clients
    $string =~ s/\x04[0-9a-fA-F]{0,6}//ig;

    # see ECMA-48 + advice by urxvt author
    $string =~ s/\x1B\[.*?[\x00-\x1F\x40-\x7E]//g;

    # strip cancellation codes too if there are no formatting codes
    $string =~ s/\x0f//g if !has_formatting($string);
    return $string;
}

sub strip_formatting {
    my ($string) = @_;
    return if !defined $string;
    $string =~ s/[\x0f\x02\x1f\x16\x1d\x11]//g;

    # strip cancellation codes too if there are no color codes
    $string =~ s/\x0f//g if !has_color($string);

    return $string;
}

sub decode_irc {
    my ($line) = @_;
    my $utf8 = guess_encoding($line, 'utf8');
    return ref $utf8 ? decode('utf8', $line) : decode('cp1252', $line);
}

sub _diff {
    my ($before, $after) = @_;
    my %in_before;
    @in_before{@$before} = ();
    my %in_after;
    @in_after{@$after} = ();
    my (@diff, %seen);

    for my $seen (@$before) {
        next if exists $seen{$seen} || exists $in_after{$seen};
        $seen{$seen} = 1;
        push @diff, ['-', $seen];
    }

    %seen = ();

    for my $seen (@$after) {
        next if exists $seen{$seen} || exists $in_before{$seen};
        $seen{$seen} = 1;
        push @diff, ['+', $seen];
    }

    return @diff;
}

1;

=encoding utf8

=head1 NAME

IRC::Utils - Common utilities for IRC-related tasks

=head1 SYNOPSIS

 use strict;
 use warnings;

 use IRC::Utils ':ALL';

 my $nickname = '^Lame|BOT[moo]';
 my $uppercase_nick = u_irc($nickname);
 my $lowercase_nick = l_irc($nickname);

 my $mode_line = 'ov+b-i Bob sue stalin*!*@*';
 my $hashref = parse_mode_line($mode_line);

 my $banmask = 'stalin*';
 my $full_banmask = parse_mask($banmask);

 if (matches_mask($full_banmask, 'stalin!joe@kremlin.ru')) {
     print "EEK!";
 }

 my $decoded = irc_decode($raw_irc_message);
 print $decoded, "\n";

 if (has_color($message)) {
    print 'COLOR CODE ALERT!\n";
 }

 my $results_hashref = matches_mask_array(\@masks, \@items_to_match_against);

 my $nick = parse_user('stalin!joe@kremlin.ru');
 my ($nick, $user, $host) = parse_user('stalin!joe@kremlin.ru');

=head1 DESCRIPTION

The functions in this module take care of many of the tasks you are faced
with when working with IRC. Mode lines, ban masks, message encoding and
formatting, etc.

=head1 FUNCTIONS

=head2 C<u_irc>

Takes one mandatory parameter, a string to convert to IRC uppercase, and one
optional parameter, the casemapping of the ircd (which can be B<'rfc1459'>,
B<'strict-rfc1459'> or B<'ascii'>. Default is B<'rfc1459'>). Returns the IRC
uppercase equivalent of the passed string.

=head2 C<l_irc>

Takes one mandatory parameter, a string to convert to IRC lowercase, and one
optional parameter, the casemapping of the ircd (which can be B<'rfc1459'>,
B<'strict-rfc1459'> or B<'ascii'>. Default is B<'rfc1459'>). Returns the IRC
lowercase equivalent of the passed string.

=head2 C<parse_mode_line>

Takes a list representing an IRC mode line. Returns a hashref. If the modeline
couldn't be parsed the hashref will be empty. On success the following keys
will be available in the hashref:

B<'modes'>, an arrayref of normalised modes;

B<'args'>, an arrayref of applicable arguments to the modes;

Example:

 my $hashref = parse_mode_line( 'ov+b-i', 'Bob', 'sue', 'stalin*!*@*' );

 # $hashref will be:
 {
    modes => [ '+o', '+v', '+b', '-i' ],
    args  => [ 'Bob', 'sue', 'stalin*!*@*' ],
 }

=head2 C<parse_mask>

Takes one parameter, a string representing an IRC mask. Returns a normalised
full mask.

Example:

 $fullbanmask = parse_mask( 'stalin*' );

 # $fullbanmask will be: 'stalin*!*@*';

=head2 C<matches_mask>

Takes two parameters, a string representing an IRC mask (it'll be processed
with L<C<parse_mask>|/parse_mask> to ensure that it is normalised)
and something to match against the IRC mask, such as a nick!user@hostname
string. Returns a true value if they match, a false value otherwise.
Optionally, one may pass the casemapping (see L<C<u_irc>|/u_irc>), as this
function uses C<u_irc> internally.

=head2 C<matches_mask_array>

Takes two array references, the first being a list of strings representing
IRC masks, the second a list of somethings to test against the masks. Returns
an empty hashref if there are no matches. Otherwise, the keys will be the
masks matched, each value being an arrayref of the strings that matched it.
Optionally, one may pass the casemapping (see L<C<u_irc>|/u_irc>), as
this function uses C<u_irc> internally.

=head2 C<unparse_mode_line>

Takes one argument, a string representing a number of mode changes. Returns
a condensed version of the changes.

  my $mode_line = unparse_mode_line('+o+o+o-v+v');
  $mode_line is now '+ooo-v+v'

=head2 C<gen_mode_change>

Takes two arguments, strings representing a set of IRC user modes before and
after a change. Returns a string representing what changed.

  my $mode_change = gen_mode_change('abcde', 'befmZ');
  $mode_change is now '-acd+fmZ'

=head2 C<parse_user>

Takes one parameter, a string representing a user in the form
nick!user@hostname. In a scalar context it returns just the nickname.
In a list context it returns a list consisting of the nick, user and hostname,
respectively.

=head2 C<is_valid_chan_name>

Takes one argument, a channel name to validate. Returns true or false if the
channel name is valid or not. You can supply a second argument, an array of
characters of allowed channel prefixes. Defaults to C<['#', '&']>.

=head2 C<is_valid_nick_name>

Takes one argument, a nickname to validate. Returns true or false if the
nickname is valid or not.

=head2 C<numeric_to_name>

Takes an IRC server numerical reply code (e.g. '001') as an argument, and
returns the corresponding name (e.g. 'RPL_WELCOME').

=head2 C<name_to_numeric>

Takes an IRC server reply name (e.g. 'RPL_WELCOME') as an argument, and returns the
corresponding numerical code (e.g. '001').

=head2 C<has_color>

Takes one parameter, a string of IRC text. Returns true if it contains any IRC
color codes, false otherwise. Useful if you want your bot to kick users for
(ab)using colors. :)

=head2 C<has_formatting>

Takes one parameter, a string of IRC text. Returns true if it contains any IRC
formatting codes, false otherwise.

=head2 C<strip_color>

Takes one parameter, a string of IRC text. Returns the string stripped of all
IRC color codes.

=head2 C<strip_formatting>

Takes one parameter, a string of IRC text. Returns the string stripped of all
IRC formatting codes.

=head2 C<decode_irc>

This function takes a byte string (i.e. an unmodified IRC message) and
and returns a text string. Since the source encoding might have been UTF-8,
you should store it with UTF-8 or some other Unicode encoding in your
file/database/whatever to be safe. For a more detailed discussion, see
L</ENCODING>.

 use IRC::Utils qw(decode_irc);

 sub message_handler {
     my ($nick, $channel, $message) = @_;

     # not wise, $message is a byte string of unkown encoding
     print $message, "\n";

     $message = decode_irc($what);

     # good, $message is a text string
     print $message, "\n";
 }

=head1 CONSTANTS

Use the following constants to add formatting and mIRC color codes to IRC
messages.

Normal text:

 NORMAL

Formatting:

 BOLD
 UNDERLINE
 REVERSE
 ITALIC
 FIXED

Colors:

 WHITE
 BLACK
 BLUE
 GREEN
 RED
 BROWN
 PURPLE
 ORANGE
 YELLOW
 LIGHT_GREEN
 TEAL
 LIGHT_CYAN
 LIGHT_BLUE
 PINK
 GREY
 LIGHT_GREY

Individual non-color formatting codes can be cancelled with their
corresponding constant, but you can also cancel all of them at once with
C<NORMAL>. To cancel the effect of color codes, you must use C<NORMAL>.
which of course has the side effect of cancelling all other formatting codes
as well.

 $msg = 'This word is '.YELLOW.'yellow'.NORMAL.' while this word is'.BOLD.'bold'.BOLD;
 $msg = UNDERLINE.BOLD.'This sentence is both underlined and bold.'.NORMAL;

=head1 ENCODING

=head2 Messages

The only encoding requirement the IRC protocol places on its messages is that
they be 8-bits and ASCII-compatible. This has resulted in most of the Western
world settling on ASCII-compatible Latin-1 (usually Microsoft's CP1252, a
Latin-1 variant) as a convention. Recently, popular IRC clients (mIRC, xchat,
certain irssi configurations) have begun sending a mixture of CP1252 and UTF-8
over the wire to allow more characters without breaking backward compatibility
(too much). They send CP1252 encoded messages if the characters fit within
that encoding, otherwise falling back to UTF-8, and likewise autodetecting
the encoding (UTF-8 or CP1252) of incoming messages. Since writing text with
mixed encoding to a file, terminal, or database is not a good idea, you need
a way to decode messages from IRC. L<C<decode_irc>|/decode_irc> will do that.

=head2 Channel names

The matter is complicated further by the fact that some servers allow
non-ASCII characters in channel names. IRC modules generally don't explicitly
encode or decode any IRC traffic, but they do have to concatenate parts of a
message (e.g. a channel name and a message) before sending it over the wire.
So when you do something like C<< privmsg($channel, 'æði') >>, where
C<$chanbel> is the unmodified channel name (a byte string) you got from an
earlier IRC message, the channel name will get double-encoded when
concatenated with the message (a non-ASCII text string) if it contains
non-ASCII bytes.

To prevent this, you can't simply call L<C<decode_irc>|/decode_irc> on the
channel name and then use it. C<'#æði'> in CP1252 is not the same channel as
C<'#æði'> in UTF-8, since they are represented as different strings of bytes.
The channel name and your message must therefore both be byte strings, or
both be text strings If they're text strings, the UTF-8 flag must be off for
both, or on for both.

A simple rule to follow is to call L<C<encode_utf8>|Encode> on any part
(channel or message) which is a text string. Here are some examples:

 use Encode qw(encode_utf8);

 sub message_handler {
     # these three are all byte strings
     my ($nick, $channel, $message) = @_;

     # bad: if $channel has any non-ASCII bytes, they will get double-encoded
     privmsg($channel, 'æði');

     # bad: if $message has any non-ASCII bytes, they will get double-encoded
     privmsg('#æði', $message);

     # good: both are byte strings already, so they will concatenate correctly
     privmsg($channel, $message);

     # good: both are text strings (Latin1 as per Perl's default), so
     # they'll be concatenated correctly
     privmsg('#æði', 'æði');

     # good: similar to the last one, except now they're using UTF-8, which
     # means that the channel is actually not the same as above
     use utf8;
     privmsg('#æði', 'æði');

     # good: $channel and $msg_bytes are both byte strings
     my $msg_bytes = encode_utf8('æði');
     privmsg($channel, $msg_bytes);

     # good: $chan_bytes and $message are both byte strings
     my $chan_bytes = encode_utf8('#æði');
     privmsg($chan_bytes, $message);
 }

See also L<Encode|Encode>, L<perluniintro|perluniintro>,
L<perlunitut|perlunitut>, L<perlunicode|perlunicode>, and
L<perlunifaq|perlunifaq>.

=head1 AUTHOR

Chris C<BinGOs> Williams <chris@bingosnet.co.uk>

Hinrik E<Ouml>rn SigurE<eth>sson <hinrik.sig@gmail.com>

=head1 SEE ALSO

L<POE::Component::IRC|POE::Component::IRC>

L<POE::Component::Server::IRC|POE::Component::Server::IRC>

=cut
