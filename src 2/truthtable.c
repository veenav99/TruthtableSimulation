#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct vars{
    char *label;
    int id; 
    struct vars *next;
} vars_t;

#define MAX_STEPS 100
#define VAR_ZERO -1
#define VAR_ONE -2

typedef enum { AND, OR, NAND, NOR, XOR, NOT, PASS, DECODER, MULTIPLEXER } kind_t;
typedef struct gate {
    kind_t kind;
    int    size;    // indicates size of DECODER and MULTIPLEXER
    int    count;   // number of params taken 
    int    *params; // length determined by kind and size;
    bool   *is_input;
    struct gate *next;
} gate_t;

typedef enum bit_t{
    UNSET , HI , LO , Z
} bit_t;

typedef struct circuit {
    int input_count, output_count;
    int *inputs , *outputs;
    int gate_count;
    vars_t *vars_head;
    gate_t *gates_tail;
    int max_id;
    gate_t *gates_head;
    bit_t *bits;
} circuit_t;

static circuit_t circuit;

bool eval_and(bool a , bool b){ //for different types of gates
    return a && b;
}

vars_t *make_var( char *word , int count ){ // this is done
    vars_t *vars = malloc( sizeof( vars_t ) );
    vars -> label = malloc( count + 1 );

    memcpy( vars -> label , word , count );
    vars -> label [ count ] = '\0';
    circuit.max_id++;
    vars -> id = circuit.max_id;
    vars -> next = circuit.vars_head;
    circuit.vars_head = vars;

    return vars;
}

void skip_whitespace( char **word ){
    while( **word == ' ' || **word == '\t' ){
        ( *word )++;
    }
}

vars_t *look_up_var( char *word , int count ){
    vars_t *var = circuit.vars_head;
    for(; var != NULL; var = var -> next ){
        if( strncmp( word , var -> label , count ) == 0  && strlen( var -> label ) == count ){
            return var;
        }
    }
    return NULL;
}

int parse_variable( char **word , bool look_up ){
    char dummy[ 1000 ];
    int count;

    skip_whitespace( word );
   
    if( sscanf( *word , "%s%n" , dummy , &count ) < 0 ){
        fprintf( stderr , "parse_variable failed\n" );
        return 0;
    }
    vars_t *var;

    if( look_up ){
        var = look_up_var( *word , count );
        if( var == NULL ){
            var = make_var( *word , count );
        } 
    } else {
        var = make_var( *word , count );
    }
    *word += count;

    skip_whitespace( word );
    return var -> id;
} 

bool parse_n_variables( char **word , int n , int *array , bool store , bool look_up ){
    for( int i = 0; i < n; i++ ){
        int id = parse_variable( word , look_up );
        if( id == 0 ){
            return false;
        }
        if( store ){
            array[ i ] = id;
        }
        if( i < n - 1 ){
            if( **word == '\n' ){
                fprintf( stderr , "parse_n_variables failed: line terminated early\n" );

                return false;
            }
        }
    }
    return true;
}

void append_gate( gate_t *gate ){
    if( circuit.gates_tail == NULL ){
        circuit.gates_tail = gate;
        circuit.gates_head = gate;
    } else {
        circuit.gates_tail -> next = gate;
        circuit.gates_tail = gate;
    }
}

gate_t *parse_simple_gate( char *line , int n , kind_t kind ){ // OR gate
    gate_t *gate = malloc( sizeof( gate_t ) );
    gate -> params = malloc( n * sizeof( int ) ); // memory for parameters
    gate -> is_input = malloc( n * sizeof( bool ) );
    gate -> kind = kind;
    gate -> size = n;
    gate -> count = n;
    gate -> next = NULL;
    append_gate( gate );

    for( int i = 0; i < n - 1; i++ ){
        gate -> is_input[ i ] = true;
    }
    gate -> is_input[ n - 1 ] = false;

    if ( !parse_n_variables( &line , n , gate -> params , true , true ) ){
        
        free( gate -> params) ;
        free( gate );
        return NULL;
    }
    return gate;
}

gate_t *parse_decoder( char *line ){ // DECODER gate
    int n; // inputs for the decoder
    int total_params; // total number of params
    int line_advance;

    sscanf( line , "%d%n" , &n , &line_advance );
    line += line_advance;
    total_params = n + ( 1<< n ); // n inputs + 2n outputs

    gate_t *gate = malloc( sizeof( gate_t ) );
    gate -> params = malloc( total_params * sizeof( int ) ); // memory for parameters
    gate -> is_input = malloc( total_params * sizeof( bool ) );
    gate -> kind = DECODER;
    gate -> size = total_params;
    gate -> count = n;
    gate -> next = NULL;
    append_gate( gate );

    for( int i = 0; i < n; i++ ){
        gate -> is_input[ i ] = true;
    }

    for( int i = n; i < ( 1 << n ) + n; i++ ){
        gate -> is_input[ i ] = false;
    }

    if ( !parse_n_variables( &line , total_params , gate -> params , true , true ) ) {
        
        free( gate -> params );
        free( gate );
        return NULL;
    }
    return gate;
}

gate_t *parse_multiplexer( char *line ){ // MULTIPLEXER gate
    int n; // selectors for the multiplexer
    int total_params; // total number of parameters
    int line_advance;

    sscanf( line , "%d%n" , &n , &line_advance );
    line += line_advance;
    total_params = ( 1<< n ) + n + 1; // 2n inputs + n selectors + 1 output

    gate_t *gate = malloc( sizeof( gate_t ) );
    gate -> params = malloc( total_params * sizeof( int ) ); 
    gate -> is_input = malloc( total_params * sizeof( bool ) );
    gate -> kind = MULTIPLEXER;
    gate -> size = total_params;
    gate -> count = n;
    gate -> next = NULL;
    append_gate( gate );

     for( int i = 0; i < total_params - 1; i++ ){
        gate -> is_input[ i ] = true;
    }
    gate -> is_input[ total_params - 1 ] = false;

    if ( !parse_n_variables( &line , total_params , gate -> params , true , true ) ) {
        free( gate -> params );
        free( gate );
        return NULL;
    }
    return gate;
}

bool parse_inputs( char *line ){
    char word[ 1000 ];
    int n;
    
    sscanf( line , "%s%n" , word , &n );
    line += n;

    if( strcmp( word , "INPUT" ) != 0 ){
        printf( "Expected input, got %s\n" , word );

        return false;
    }
    sscanf( line , "%d%n" , &circuit.input_count , &n );
    line += n;

    circuit.inputs = malloc( sizeof( int ) * circuit.input_count );
    return parse_n_variables( &line , circuit.input_count , circuit.inputs , true , false );
}

bool parse_outputs( char *line ){
    char word[ 1000 ];
    int n;
    
    sscanf( line , "%s%n" , word , &n );
    line += n;

    if( strcmp( word , "OUTPUT" ) != 0 ){
        printf( "Expected output, got %s\n" , word );

        return false;
    }
    sscanf( line , "%d%n" , &circuit.output_count , &n );
    line += n;

    circuit.outputs = malloc( sizeof( int ) * circuit.output_count );
    return parse_n_variables( &line , circuit.output_count , circuit.outputs , true , false );
}

bool parse_gate( char *line ){
    char word[ 1000 ];
    int n;
    
    sscanf( line , "%s%n" , word , &n );
    line += n;

    if( strcmp( word , "AND" ) == 0 ){
        return parse_simple_gate( line , 3 , AND );
    } else if( strcmp( word , "OR" ) == 0 ){
        return parse_simple_gate( line , 3 , OR );
    } else if( strcmp( word , "NAND" ) == 0 ){
        return parse_simple_gate( line , 3 , NAND );
    } else if( strcmp( word , "NOR" ) == 0 ){
        return parse_simple_gate( line , 3 , NOR );
    } else if( strcmp( word , "XOR" ) == 0 ){
        return parse_simple_gate( line , 3 , XOR );
    } else if( strcmp( word , "NOT" ) == 0 ){
        return parse_simple_gate( line , 2 , NOT );
    } else if( strcmp( word , "PASS" ) == 0 ){
       return parse_simple_gate( line , 2 , PASS );
    } else if( strcmp( word , "DECODER" ) == 0 ){
        return parse_decoder( line );
    } else if( strcmp( word , "MULTIPLEXER" ) == 0 ){
        return parse_multiplexer( line );
    } else {
        printf( "Unrecognized gate %s\n" , word );
        return false;
    } 
}
void print_vartable(){
    vars_t *cur = circuit.vars_head; 
    for(; cur != NULL; cur = cur -> next ){
        printf( "%s: \t%d\n" , cur -> label , cur -> id );  
    }
}

void print_nvars( int *array , int count ){
    for( int i = 0; i < count; i++ ){
        printf( "%d " , array[ i ] );
    }
    printf( "\n" );
}

void init_vars(){
    vars_t *var = malloc( sizeof( vars_t ) );
    var -> label = strdup( "0" );
    var -> id = VAR_ZERO;
    var -> next = circuit.vars_head;
    circuit.vars_head = var;

    var = malloc( sizeof( vars_t ) );
    var -> label = strdup( "1" );
    var -> id = VAR_ONE;
    var -> next = circuit.vars_head;
    circuit.vars_head = var;
}
 
 
char *kind_to_string( kind_t kind ){
    switch( kind ){
        case AND: return "AND";
        case OR: return "OR";
        case NAND: return "NAND";
        case NOR: return "NOR";
        case XOR: return "XOR";
        case NOT: return "NOT";
        case PASS: return "PASS";
        case DECODER: return "DECODER";
        case MULTIPLEXER: return "MULTIPLEXER";
    }
}

void print_gate( gate_t * gate ){
    printf( "%s ", kind_to_string( gate -> kind ) );
    print_nvars( gate -> params , gate -> size );
}

void print_circuit(){
    print_vartable();

    printf( "INPUT " );
    print_nvars( circuit.inputs , circuit.input_count );
    printf( "OUTPUT " );
    print_nvars( circuit.outputs , circuit.output_count );

    for( gate_t *gate = circuit.gates_head; gate != 0; gate = gate -> next ){
        print_gate( gate );
    }
}

void make_circuit( FILE *file ){
    
    circuit.input_count = 0;
    circuit.output_count = 0;
    circuit.inputs = NULL;
    circuit.outputs = NULL;
    circuit.gate_count = 0;
    circuit.max_id = 0;
    circuit.gates_tail = NULL;
    circuit.vars_head = NULL;
    circuit.gates_head = NULL;
    init_vars();

    char line[ 1000 ];
    
    if( fscanf( file , "%[^\n] " , line ) != EOF ) {
        parse_inputs( line );
    }

    if( fscanf( file , "%[^\n] " , line ) != EOF ) {
        parse_outputs( line );
    }

    while( fscanf( file , "%[^\n] " , line ) != EOF ) {
        parse_gate( line );
    }
    circuit.bits = malloc( sizeof( bit_t ) * ( circuit.max_id + 3 ) );
    circuit.bits += 2;
    circuit.bits[ VAR_ZERO ] = LO;
    circuit.bits[ VAR_ONE ] = HI;
}

void clear_values( bit_t *bits , int n , bit_t val ){
    for( int i = 0; i < n; i++ ){
        bits[ i ] = val;
    }
}

bool all_set( bit_t *bits , int n ){
    for( int i = 0; i < n; i++ ){
        if( bits[ i ] != HI ){
            return false;
        }
    }
    return true;
}

void increment_bits( bit_t *bits , int n ){
    int carry = 1; 

    for( int i = n - 1; i >= 0; i-- ){
        if( bits[ i ] == HI ){
            carry += 1;;
        } 
        if( carry % 2 == 1 ){
            bits[ i ] = HI;
        } else {
            bits[ i ] = LO;
        }
        carry /= 2;
    }
}

void print_truthtable_line(){
    for( int i = 0; i < circuit.input_count; i++ ){
        int index = circuit.inputs[ i ];

        if( circuit.bits[ index ] == HI ){
            printf( "1 " );
        } else {
            printf( "0 ");
        }
    }
    printf( "|" );

    for( int i = 0; i < circuit.output_count; i++ ){
        int index = circuit.outputs[ i ];
        if( circuit.bits[ index ] == HI ){
            printf( " 1");
        } else {
            printf( " 0");
        }
    }
    printf( "\n" );
}

void step_decoder( gate_t *gate , bit_t *bits ){
    int num_inputs = gate -> count;
    int num_outputs = gate -> size - num_inputs;

    int input_combination = 0;
        
    for ( int j = 0; j < num_inputs; j++ ) {
        if( bits[ j ] == HI ){
            input_combination |= 1;
        }
        input_combination <<= 1;
    }
    input_combination >>= 1;

    int decoder_start_index = num_inputs;

    for ( int j = 0; j < num_outputs; j++ ) {
        int output_index = decoder_start_index + j;

        bits[ output_index ] = UNSET; 

        if ( input_combination == j ) {
            bits[ output_index ] = HI;
        } else {
            bits[ output_index ] = LO;
        }
    }
}

void step_multiplexer( gate_t *gate , bit_t *bits ){
    int num_selectors = gate -> count;
    int num_data_inputs = gate -> size - num_selectors - 1;
    int selector_value = 0;

    for ( int j = 0; j <  num_selectors; j++ ) {
        if( bits[ j + num_data_inputs ] == HI ){
            selector_value |= 1;
        }
        selector_value <<= 1;
    }
    selector_value >>= 1;

    int output_index = num_selectors + num_data_inputs;

    bits[ output_index ] = bits[ selector_value ];
}

bool step_gate( gate_t *gate ){ 
    bit_t *bits = malloc( sizeof( bit_t ) * gate -> size );
    for( int i = 0; i < gate -> size; i++ ){
        bits[ i ] = circuit.bits[ gate -> params[ i ] ]; 
    }
    for( int i = 0; i < gate -> size; i++ ){
        if( gate -> is_input[ i ] && bits[ i ] == UNSET ){
            return false;
        }
    }

    switch( gate -> kind ){
        case AND:
        if( bits[ 0 ] == HI && bits[ 1 ] == HI ){
            bits[ 2 ] = HI;
        } else {
            bits[ 2 ] = LO;
        }
        break;
        case OR:
        if( bits[ 0 ] == HI || bits[ 1 ] == HI ){
            bits[ 2 ] = HI;
        } else {
            bits[ 2 ] = LO;
        }
        break;
        case NAND: 
        if( bits[ 0 ] == HI && bits[ 1 ] == HI ){
            bits[ 2 ] = LO;
        } else {
            bits[ 2 ] = HI;
        }
        break;
        case NOR: 
        if( bits[ 0 ] == HI || bits[ 1 ] == HI ){
            bits[ 2 ] = LO;
        } else {
            bits[ 2 ] = HI;
        }
        break;

        case XOR:
        if( bits[ 0 ] != bits[ 1 ] ){
            bits[ 2 ] = HI;
        } else {
            bits[ 2 ] = LO;
        }
        break;

        case NOT: 
        if( bits[ 0 ] == HI ){
            bits[ 1 ] = LO;
        } else {
            bits[ 1 ] = HI;
        }
        break;

        case PASS:
        bits[ 1 ] = bits[ 0 ];
        break;

        case DECODER: 
        step_decoder( gate , bits );
        break;

        case MULTIPLEXER: 
        step_multiplexer( gate , bits );
        break;
    }
    for( int i = 0; i < gate -> size; i++ ){
        if( !gate -> is_input[ i ] ){
            circuit.bits[ gate -> params[ i ] ] = bits[ i ];
        }
        
    }
    free( bits );
    return true;
}

bool circuit_step(){ 
    for( gate_t *gate = circuit.gates_head; gate; gate = gate -> next ){
        step_gate( gate );
    }
    // return true if there is more work to be done
    for( int i = 1; i <= circuit.max_id; i++ ){ 
        if( circuit.bits[ i ] == UNSET ){
            return true; 
        }
    }
    return false;
}

void print_truthtable( ){
    bit_t *inputs = malloc( sizeof( bit_t ) * circuit.input_count );
    clear_values( inputs , circuit.input_count , LO );

    for( int j = 0; j < ( 1<< circuit.input_count ); j++){
        clear_values( circuit.bits , circuit.max_id + 1 , UNSET );
        for( int i = 0; i < circuit.input_count; i++ ){
            circuit.bits[ circuit.inputs[ i ] ] = inputs[ i ];
        }
        for( int i = 0; i < MAX_STEPS; i++ ){
           if( !circuit_step() ){
            break;
           }
        }
        print_truthtable_line();
        increment_bits( inputs , circuit.input_count );
    }   
}

int main( int argc , char **argv ){
    if( argc == 1 ){
        make_circuit( stdin );
    } else if( argc == 2 ) { 
        FILE *file = fopen( argv[ 1 ] , "rb" );
        if( file == NULL ){
            perror( "failed to open the file" );
            return 1;
        }
        make_circuit( file );
        fclose( file );
    } 
    //print_circuit(); 
    print_truthtable();
}