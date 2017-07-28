drop database if exists pictodash;
create database pictodash;

drop user if exists puser;
create user puser with password 'test';

grant all privileges on database pictodash to puser;

\c pictodash;
set role puser;

/* Usuarios */
drop table if exists usuarios cascade;
create table usuarios(
    id serial primary key,
    usuario text,
    contrasena text,
    email text,
    nombres text,
    apellido_paterno text,
    apellido_materno text,
    permiso_administrador boolean,
    permiso_tablero boolean
);

insert into usuarios ("usuario","contrasena","nombres","apellido_paterno","apellido_materno","permiso_administrador", "permiso_tablero") values
('admin','$2a$10$DmxbjTLBYDdcha8qlXpsaOyUqkJ0BAQ3Q4EIyMtr5HLXm6R0gSvbm','Administrador','','', true, true);
